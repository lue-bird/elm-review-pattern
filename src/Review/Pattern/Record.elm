module Review.Pattern.Record exposing (forbid)

{-| Forbid `{ field, ... }` record patterns.

@docs forbid


## why

  - Readability and error prone-ness: Similar to exposing from an import, it becomes hard to follow where a certain value originated from

        checkForConcatOnMap { nodeRange, firstArg } =
            case toSpecificCall ( "List", "map" ) firstArg of
                Just { fnRange } ->
                    let ...
                    in
                    Fix.replaceByExpression nodeRange firstArg
                        ++ Fix.replace fnRange "List.concatMap"

                Nothing ->
                    Nothing

    Since `fnRange`, `nodeRange` and `firstArg` could have all come from either call,
    it's really hard to trace their origin without jumping around in your code.

        checkForConcatOnMap concatCall =
            case toSpecificCall ( "List", "map" ) concatCall.firstArg of
                Just mapCall ->
                    let ...
                    in
                    Fix.replaceByExpression concatCall.nodeRange concatCall.firstArg
                        ++ Fix.replace mapCall.fnRange "List.concatMap"

                Nothing ->
                    Nothing

    Here it's _always_ very clear what thing a field refers to. Don't you agree this is much easier to understand?

  - less syntax to learn to use right and keep in your head

  - Name clashes: Using record destructuring relies on the assumption that no values
    with the same name will _ever_ in the lifetime of this file be introduced.

        module Ui exposing (icon)

        icon { image, style } =
            ...

    future iteration

        module Ui exposing (icon, image)

        image { proportions, url } =
            ...

        icon { image, style } =
            ...

    Ok, we have to change this one completely unrelated field into a record access.
    That's not too bad.

        module Ui exposing (icon, image)

        image { proportions, url } =
            ...

        icon ({ style } as config) =
            ...

    future iteration

        module Ui exposing (icon, image)

        image { proportions, url } =
            ...

        icon ({ style } as config) =
            ...

        url { path, openInNewTab } =
            ...

    Oh well, another clash

        module Ui exposing (icon, image)

        image ({ proportions } as config) =
            ...

        icon ({ style } as config) =
            ...

        url { path, openInNewTab } =
            ...

    This mixed use of record access and record destructuring is quite ugly I'd say.
    Maybe just use record access consistently so you don't have these problems?

  - Scalability: You can't really use record destructuring anywhere except at the most outer argument/let level
    because the field names quickly lose context and introduce name clashes.

        checkForConcatOnMap { nodeRange, firstArg } =
            case toSpecificCall ( "List", "map" ) firstArg of
                Just { fnRange, nodeRange } ->
                    let ...
                    in
                    Fix.replaceByExpression nodeRange nodeRange -- well...
                        ++ Fix.replace fnRange "List.concatMap"

                Nothing ->
                    Nothing

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import ElmExpression.Extra
import ElmPattern.Extra
import Review.Fix
import Review.Rule as Rule exposing (Rule)


{-| Forbid `{ field, ... }` record patterns.

    config =
        [ Review.Pattern.Record.forbid
        ]

For example

    a ({ field } as record) =
        f field record

will be fixed to

    a record =
        f record.field record

Patterns without `as` will not get an auto-fix, so either manually apply this change
or add a temporary `as`.

-}
forbid : Rule
forbid =
    Rule.newModuleRuleSchemaUsingContextCreator "Review.Pattern.Record" initialContext
        |> Rule.providesFixesForModuleRule
        |> Rule.withDeclarationEnterVisitor (\decl context -> ( visitDeclaration decl, context ))
        |> Rule.withExpressionEnterVisitor (\expr context -> ( visitExpression expr, context ))
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            {}
        )


visitExpression : Node Expression -> List (Rule.Error {})
visitExpression expressionNode =
    case Elm.Syntax.Node.value expressionNode of
        Elm.Syntax.Expression.LambdaExpression lambda ->
            lambda.args
                |> List.concatMap
                    (checkPattern { intoExpression = lambda.expression })

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            List.concatMap
                (\( casePatternNode, caseExpression ) ->
                    casePatternNode |> checkPattern { intoExpression = caseExpression }
                )
                caseBlock.cases

        Elm.Syntax.Expression.LetExpression letBlock ->
            List.concatMap
                (\(Node _ letDeclaration) ->
                    case letDeclaration of
                        Elm.Syntax.Expression.LetFunction letFunctionOrValueDeclaration ->
                            List.concatMap
                                (checkPattern
                                    { intoExpression = (Elm.Syntax.Node.value letFunctionOrValueDeclaration.declaration).expression }
                                )
                                (Elm.Syntax.Node.value letFunctionOrValueDeclaration.declaration).arguments

                        Elm.Syntax.Expression.LetDestructuring letDestructuringPattern _ ->
                            letDestructuringPattern
                                |> checkPattern { intoExpression = letBlock.expression }
                )
                letBlock.declarations

        _ ->
            []


visitDeclaration : Node Declaration -> List (Rule.Error {})
visitDeclaration (Node _ declaration) =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            (Elm.Syntax.Node.value functionDeclaration.declaration).arguments
                |> List.concatMap
                    (checkPattern
                        { intoExpression = (Elm.Syntax.Node.value functionDeclaration.declaration).expression }
                    )

        _ ->
            []


checkPattern :
    { intoExpression : Node Expression }
    -> Node Pattern
    -> List (Rule.Error {})
checkPattern config patternNode =
    case patternNode of
        Node recordPatternRange (Elm.Syntax.Pattern.RecordPattern _) ->
            [ Rule.error
                { message = "record pattern is forbidden"
                , details =
                    [ "You can replace this pattern by a variable and convert each use of a destructured field into a record access (`variable.field`). If you add `as yourRecordName`, I can automatically fix this."
                    , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                    ]
                }
                recordPatternRange
            ]

        Node asPatternRange (Elm.Syntax.Pattern.AsPattern (Node recordPatternRange (Elm.Syntax.Pattern.RecordPattern fields)) (Node _ variable)) ->
            [ if fields |> List.any (\(Node _ field) -> isSpecificVariableUsedInRecordSetter field config.intoExpression) then
                Rule.error
                    { message = "record pattern is forbidden"
                    , details =
                        [ "You can replace this pattern by a variable and convert each use of a destructured field into a record access (`variable.field`)."
                        , "Right now, I can't provide this as a fix because a field variable is used in a record setter { field | ... } and these don't allow { variable.field | ... }. Maybe use a let for this specific field?"
                        , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                        ]
                    }
                    recordPatternRange

              else
                Rule.errorWithFix
                    { message = "record pattern is forbidden"
                    , details =
                        [ "You can replace this pattern by the variable after `as` and convert each use of a destructured field into a record access (`variable.field`)."
                        , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                        ]
                    }
                    recordPatternRange
                    (Review.Fix.replaceRangeBy asPatternRange variable
                        :: (fields
                                |> List.concatMap
                                    (\(Node _ field) ->
                                        config.intoExpression
                                            |> specificVariableUses field
                                            |> List.map (\useRange -> Review.Fix.replaceRangeBy useRange (variable ++ ".field"))
                                    )
                           )
                    )
            ]

        Node _ nonRecordPattern ->
            nonRecordPattern |> ElmPattern.Extra.sub |> List.concatMap (checkPattern config)


isSpecificVariableUsedInRecordSetter : String -> Node Expression -> Bool
isSpecificVariableUsedInRecordSetter specificVariableName (Node _ expression) =
    case expression of
        Elm.Syntax.Expression.RecordUpdateExpression (Node _ recordToUpdate) setters ->
            (recordToUpdate == specificVariableName)
                || (setters |> List.any (\(Node _ ( _, newValue )) -> newValue |> isSpecificVariableUsedInRecordSetter specificVariableName))

        notDirectlyReplaceable ->
            notDirectlyReplaceable |> ElmExpression.Extra.sub |> List.any (\sub -> sub |> isSpecificVariableUsedInRecordSetter specificVariableName)


specificVariableUses : String -> Node Expression -> List Range
specificVariableUses specificVariableName (Node expressionRange expression) =
    case expression of
        Elm.Syntax.Expression.FunctionOrValue [] variableName ->
            if variableName == specificVariableName then
                [ expressionRange ]

            else
                []

        notDirectlyReplaceable ->
            notDirectlyReplaceable
                |> ElmExpression.Extra.sub
                |> List.concatMap (\sub -> sub |> specificVariableUses specificVariableName)
