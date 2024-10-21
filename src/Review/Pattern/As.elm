module Review.Pattern.As exposing (forbid)

{-| Forbid `... as ..variable..` patterns.

@docs forbid


## why

  - `as` can quickly lead to barely readable, long patterns:

        Node _ (Expression.Lambda ({ argumentPatterns, resultExpression, arrowRange } as lambda)) ->
            x

    compared to

        Node _ (Expression.Lambda lambda) ->
            let
                { argumentPatterns, resultExpression, arrowRange } =
                    lambda
            in
            x

  - less syntax to learn to use right and keep in your head

  - `as` in `case of` patterns can create variables with types that know less than you:

        highestScoreUi ... =
            case computeScores ... of
                [] ->
                    textUi "no scores, yet"

                (head :: _) as scores ->
                    case List.maximum scores of
                        -- This can never happen
                        Nothing ->
                            -- so we'll just use the head
                            scoreUi head

                        Just highestFound ->
                            scoreUi highestFound

    compared to

        highestScoreUi ... =
            case computeScores ... of
                [] ->
                    textUi "no scores, yet"

                scoresHead :: scoresTail ->
                    scoreUi (List.Nonempty.maximum ( scoresHead, scoresTail ))

    There are many more reasons for not introducing variables like that, see [`VariablesBetweenCaseOf.AccessInCases.forbid`](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/latest/#why).

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import ElmPattern.Extra
import Review.Fix
import Review.ModuleNameLookupTable
import Review.Project.Dependency
import Review.Rule
import Set exposing (Set)


{-| Forbid `... as ..variable..` patterns.

    config =
        [ Review.Pattern.As.forbid
        ]

For example

    view ({ windowSize } as model) =
        Html.text "Hello!"

will be fixed to

    view model =
        let
            { windowSize } =
                model
        in
        Html.text "Hello!"

This fix doesn't try to merge this destructuring into existing lets.
Adding [`jfmengels/elm-review-simplify`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-simplify/latest)
will do this for you.

-}
forbid : Review.Rule.Rule
forbid =
    Review.Rule.newProjectRuleSchema "Review.Pattern.As.forbid" initialProjectContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withModuleVisitor
            (\moduleSchema ->
                moduleSchema
                    |> Review.Rule.withDeclarationListVisitor
                        (\declarationList context ->
                            ( [], declarationList |> List.foldl visitDeclarationForSingleVariant context )
                        )
                    |> Review.Rule.withExpressionEnterVisitor
                        (\expressionNode context ->
                            ( expressionVisitor expressionNode context, context )
                        )
                    |> Review.Rule.withDeclarationEnterVisitor
                        (\declarationNode context ->
                            ( visitDeclaration declarationNode context, context )
                        )
            )
        |> Review.Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = projectContextToModule
            , fromModuleToProject = moduleContextToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withDirectDependenciesProjectVisitor
            (\dependencies context ->
                ( []
                , visitDependencies dependencies context
                )
            )
        |> Review.Rule.fromProjectRuleSchema


visitDependencies : Dict String Review.Project.Dependency.Dependency -> ProjectContext -> ProjectContext
visitDependencies dependencies context =
    { context
        | singleVariants =
            Set.union
                (dependencies
                    |> Dict.values
                    |> List.concatMap
                        (\dependency ->
                            dependency
                                |> Review.Project.Dependency.modules
                                |> List.concatMap
                                    (\moduleDocs ->
                                        moduleDocs.unions
                                            |> List.filterMap
                                                (\choiceTypeDocs ->
                                                    case choiceTypeDocs.tags of
                                                        ( singleVariantName, _ ) :: [] ->
                                                            Just ( moduleDocs.name |> stringToModuleName, singleVariantName )

                                                        _ ->
                                                            Nothing
                                                )
                                    )
                        )
                    |> Set.fromList
                )
                context.singleVariants
    }


type alias ProjectContext =
    { singleVariants : Set ( Elm.Syntax.ModuleName.ModuleName, String )
    }


type alias ModuleContext =
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , moduleNameLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    , sourceCodeInRange : Elm.Syntax.Range.Range -> String
    , importSingleVariants : Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , moduleSingleVariants : Set String
    }


moduleContextSingleVariants : ModuleContext -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
moduleContextSingleVariants context =
    Set.union
        (context.moduleSingleVariants |> Set.map (\name -> ( context.moduleName, name )))
        context.importSingleVariants


initialProjectContext : ProjectContext
initialProjectContext =
    { singleVariants = Set.empty }


moduleContextToProject : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleContextToProject =
    Review.Rule.initContextCreator
        (\moduleContext ->
            { singleVariants = moduleContext |> moduleContextSingleVariants }
        )


projectContextToModule : Review.Rule.ContextCreator ProjectContext ModuleContext
projectContextToModule =
    Review.Rule.initContextCreator
        (\moduleName moduleNameLookup sourceCodeInRange projectContext ->
            { moduleName = moduleName
            , moduleNameLookup = moduleNameLookup
            , sourceCodeInRange = sourceCodeInRange
            , importSingleVariants = projectContext.singleVariants
            , moduleSingleVariants = Set.empty
            }
        )
        |> Review.Rule.withModuleName
        |> Review.Rule.withModuleNameLookupTable
        |> Review.Rule.withSourceCodeExtractor


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts context0 context1 =
    { singleVariants = Set.union context0.singleVariants context1.singleVariants
    }


visitDeclaration : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration -> ModuleContext -> List (Review.Rule.Error {})
visitDeclaration (Elm.Syntax.Node.Node _ declaration) context =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            (Elm.Syntax.Node.value functionDeclaration.declaration).arguments
                |> List.concatMap
                    (checkPattern
                        { intoExpression = (Elm.Syntax.Node.value functionDeclaration.declaration).expression
                        , moduleSingleVariants = context.moduleSingleVariants
                        , importSingleVariants = context.importSingleVariants
                        , sourceCodeInRange = context.sourceCodeInRange
                        , moduleNameLookup = context.moduleNameLookup
                        }
                    )

        _ ->
            []


visitDeclarationForSingleVariant : Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration -> ModuleContext -> ModuleContext
visitDeclarationForSingleVariant (Elm.Syntax.Node.Node _ declaration) context =
    case declaration of
        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            case choiceTypeDeclaration.constructors of
                (Elm.Syntax.Node.Node _ singleVariant) :: [] ->
                    { context
                        | moduleSingleVariants =
                            context.moduleSingleVariants |> Set.insert (Elm.Syntax.Node.value singleVariant.name)
                    }

                _ ->
                    context

        _ ->
            context


expressionVisitor : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> ModuleContext -> List (Review.Rule.Error {})
expressionVisitor expression context =
    case Elm.Syntax.Node.value expression of
        Elm.Syntax.Expression.LambdaExpression lambda ->
            List.concatMap
                (checkPattern
                    { intoExpression = lambda.expression
                    , moduleSingleVariants = context.moduleSingleVariants
                    , importSingleVariants = context.importSingleVariants
                    , sourceCodeInRange = context.sourceCodeInRange
                    , moduleNameLookup = context.moduleNameLookup
                    }
                )
                lambda.args

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            List.concatMap
                (\( casePatternNode, caseExpression ) ->
                    casePatternNode
                        |> checkPattern
                            { intoExpression = caseExpression
                            , moduleSingleVariants = context.moduleSingleVariants
                            , importSingleVariants = context.importSingleVariants
                            , sourceCodeInRange = context.sourceCodeInRange
                            , moduleNameLookup = context.moduleNameLookup
                            }
                )
                caseBlock.cases

        Elm.Syntax.Expression.LetExpression letBlock ->
            List.concatMap
                (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                    case letDeclaration of
                        Elm.Syntax.Expression.LetFunction letFunctionOrValueDeclaration ->
                            List.concatMap
                                (checkPattern
                                    { intoExpression = (Elm.Syntax.Node.value letFunctionOrValueDeclaration.declaration).expression
                                    , moduleSingleVariants = context.moduleSingleVariants
                                    , importSingleVariants = context.importSingleVariants
                                    , sourceCodeInRange = context.sourceCodeInRange
                                    , moduleNameLookup = context.moduleNameLookup
                                    }
                                )
                                (Elm.Syntax.Node.value letFunctionOrValueDeclaration.declaration).arguments

                        Elm.Syntax.Expression.LetDestructuring letDestructuringPattern _ ->
                            letDestructuringPattern
                                |> checkPattern
                                    { intoExpression = letBlock.expression
                                    , moduleSingleVariants = context.moduleSingleVariants
                                    , importSingleVariants = context.importSingleVariants
                                    , sourceCodeInRange = context.sourceCodeInRange
                                    , moduleNameLookup = context.moduleNameLookup
                                    }
                )
                letBlock.declarations

        _ ->
            []


checkPattern :
    { intoExpression : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    , importSingleVariants : Set ( Elm.Syntax.ModuleName.ModuleName, String )
    , moduleSingleVariants : Set String
    , sourceCodeInRange : Elm.Syntax.Range.Range -> String
    , moduleNameLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> List (Review.Rule.Error {})
checkPattern config patternNode =
    case patternNode of
        Elm.Syntax.Node.Node patternRange (Elm.Syntax.Pattern.AsPattern patternInAs (Elm.Syntax.Node.Node variableRange variable)) ->
            case patternInAs |> patternKind config of
                Narrowing ->
                    [ Review.Rule.error
                        { message = "as pattern is forbidden"
                        , details =
                            [ "The variable introduced in this ... as " ++ variable ++ " pattern has a broader type than the case allows. It's best to convert the narrow values into a broad type as late as possible or not at all."
                            , "Using `as` in general is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                            ]
                        }
                        variableRange
                    ]

                Destructuring ->
                    [ Review.Rule.errorWithFix
                        { message = "as pattern is forbidden"
                        , details =
                            [ "You can replace this ... as " ++ variable ++ " pattern by a let."
                            , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                            ]
                        }
                        variableRange
                        [ Review.Fix.replaceRangeBy patternRange variable
                        , let
                            intoExpressionStart : Elm.Syntax.Range.Location
                            intoExpressionStart =
                                (Elm.Syntax.Node.range config.intoExpression).start

                            expressionIndentation : String
                            expressionIndentation =
                                String.repeat (intoExpressionStart.column - 1) " "
                          in
                          Review.Fix.insertAt intoExpressionStart
                            ([ "let\n"
                             , expressionIndentation
                             , "    "
                             , config.sourceCodeInRange (patternInAs |> Elm.Syntax.Node.range)
                             , " =\n"
                             , expressionIndentation
                             , "        "
                             , variable
                             , "\n"
                             , expressionIndentation
                             , "in\n"
                             , expressionIndentation
                             ]
                                |> String.concat
                            )
                        ]
                    ]

        Elm.Syntax.Node.Node _ nonAsPattern ->
            nonAsPattern |> ElmPattern.Extra.sub |> List.concatMap (checkPattern config)


type PatternKind
    = Destructuring
    | Narrowing


patternKind :
    { context
        | importSingleVariants : Set ( Elm.Syntax.ModuleName.ModuleName, String )
        , moduleSingleVariants : Set String
        , moduleNameLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> PatternKind
patternKind context (Elm.Syntax.Node.Node patternRange pattern) =
    let
        patternListKind : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern) -> PatternKind
        patternListKind =
            \patterns ->
                if patterns |> List.any (\partPattern -> (partPattern |> patternKind context) == Narrowing) then
                    Narrowing

                else
                    Destructuring
    in
    case pattern of
        Elm.Syntax.Pattern.ListPattern _ ->
            Narrowing

        Elm.Syntax.Pattern.TuplePattern partPatterns ->
            partPatterns |> patternListKind

        Elm.Syntax.Pattern.RecordPattern _ ->
            Destructuring

        Elm.Syntax.Pattern.NamedPattern variantRef patterns ->
            case Review.ModuleNameLookupTable.moduleNameAt context.moduleNameLookup patternRange of
                Nothing ->
                    Narrowing

                Just [] ->
                    if context.moduleSingleVariants |> Set.member variantRef.name then
                        patterns |> patternListKind

                    else
                        Narrowing

                Just (modulePart0 :: modulePart1Up) ->
                    if context.importSingleVariants |> Set.member ( modulePart0 :: modulePart1Up, variantRef.name ) then
                        patterns |> patternListKind

                    else
                        Narrowing

        Elm.Syntax.Pattern.UnConsPattern _ _ ->
            Narrowing

        Elm.Syntax.Pattern.AsPattern patternInAs _ ->
            patternInAs |> patternKind context

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternKind context

        Elm.Syntax.Pattern.VarPattern _ ->
            Destructuring

        Elm.Syntax.Pattern.AllPattern ->
            Destructuring

        Elm.Syntax.Pattern.UnitPattern ->
            Destructuring

        Elm.Syntax.Pattern.CharPattern _ ->
            Narrowing

        Elm.Syntax.Pattern.StringPattern _ ->
            Narrowing

        Elm.Syntax.Pattern.IntPattern _ ->
            Narrowing

        Elm.Syntax.Pattern.HexPattern _ ->
            Narrowing

        Elm.Syntax.Pattern.FloatPattern _ ->
            Narrowing


stringToModuleName : String -> List String
stringToModuleName moduleNameString =
    moduleNameString |> String.split "."
