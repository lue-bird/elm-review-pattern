module Review.Pattern.Record exposing (forbid)

{-| Forbid `{ field, ... }` record patterns.

@docs forbid


## why

  - readability and error prone-ness: Similar to exposing from an import, it becomes hard to follow where a certain value originated from

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

  - name clashes: Using record destructuring relies on the assumption that no values
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

        module Ui exposing (icon, image, url)

        image { proportions, url } =
            ...

        icon ({ style } as config) =
            ...

        url { path, openInNewTab } =
            ...

    Oh well, another clash

        module Ui exposing (icon, image, url)

        image ({ proportions } as config) =
            ...

        icon ({ style } as config) =
            ...

        url { path, openInNewTab } =
            ...

    This mixed use of record access and record destructuring is quite ugly I'd say.
    Maybe just use record access consistently so you don't have these problems?

  - leaves potential future need to refactor: You can't really use record destructuring anywhere except at the most outer argument/let level
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

import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import ElmExpression.Extra
import ElmPattern.Extra
import Review.Fix
import Review.Rule


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
forbid : Review.Rule.Rule
forbid =
    Review.Rule.newModuleRuleSchemaUsingContextCreator "Review.Pattern.Record" initialContext
        |> Review.Rule.providesFixesForModuleRule
        |> Review.Rule.withDeclarationEnterVisitor (\decl context -> ( visitDeclaration decl, context ))
        |> Review.Rule.withExpressionEnterVisitor (\expr context -> ( visitExpression expr, context ))
        |> Review.Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Review.Rule.ContextCreator () Context
initialContext =
    Review.Rule.initContextCreator
        (\() ->
            {}
        )


visitExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> List (Review.Rule.Error {})
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
                (\(Elm.Syntax.Node.Node _ letDeclaration) ->
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


visitDeclaration : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration -> List (Review.Rule.Error {})
visitDeclaration (Elm.Syntax.Node.Node _ declaration) =
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
    { intoExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression }
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> List (Review.Rule.Error {})
checkPattern config patternNode =
    case patternNode of
        Elm.Syntax.Node.Node recordPatternRange (Elm.Syntax.Pattern.RecordPattern _) ->
            [ Review.Rule.error
                { message = "record pattern is forbidden"
                , details =
                    [ "You can replace this pattern by a variable and convert each use of a destructured field into a record access (`variable.field`). If you add `as yourRecordName`, I can automatically fix this."
                    , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                    ]
                }
                recordPatternRange
            ]

        Elm.Syntax.Node.Node asPatternRange (Elm.Syntax.Pattern.AsPattern (Elm.Syntax.Node.Node recordPatternRange (Elm.Syntax.Pattern.RecordPattern fields)) (Elm.Syntax.Node.Node _ variable)) ->
            [ if fields |> List.any (\(Elm.Syntax.Node.Node _ field) -> isSpecificVariableUsedInRecordSetter field config.intoExpression) then
                Review.Rule.error
                    { message = "record pattern is forbidden"
                    , details =
                        [ "You can replace this pattern by a variable and convert each use of a destructured field into a record access (`variable.field`)."
                        , "Right now, I can't provide this as a fix because a field variable is used in a record setter { field | ... } and these don't allow { variable.field | ... }. Maybe use a let for this specific field?"
                        , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                        ]
                    }
                    recordPatternRange

              else
                Review.Rule.errorWithFix
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
                                    (\(Elm.Syntax.Node.Node _ field) ->
                                        config.intoExpression
                                            |> specificVariableUses field
                                            |> List.map
                                                (\useRange ->
                                                    Review.Fix.replaceRangeBy useRange (variable ++ "." ++ field)
                                                )
                                    )
                           )
                    )
            ]

        Elm.Syntax.Node.Node _ nonRecordPattern ->
            nonRecordPattern |> ElmPattern.Extra.sub |> List.concatMap (checkPattern config)


isSpecificVariableUsedInRecordSetter : String -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Bool
isSpecificVariableUsedInRecordSetter specificVariableName (Elm.Syntax.Node.Node _ expression) =
    case expression of
        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ recordToUpdate) setters ->
            (recordToUpdate == specificVariableName)
                || (setters |> List.any (\(Elm.Syntax.Node.Node _ ( _, newValue )) -> newValue |> isSpecificVariableUsedInRecordSetter specificVariableName))

        notDirectlyReplaceable ->
            notDirectlyReplaceable |> ElmExpression.Extra.sub |> List.any (\sub -> sub |> isSpecificVariableUsedInRecordSetter specificVariableName)


specificVariableUses : String -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> List Elm.Syntax.Range.Range
specificVariableUses specificVariableName (Elm.Syntax.Node.Node expressionRange expression) =
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
