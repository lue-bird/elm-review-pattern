module Review.Pattern.As exposing (forbid)

{-| Forbid `... as ..variable..` patterns.

@docs forbid

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import ElmPattern.Extra
import Review.Fix
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Forbid `... as ..variable..` patterns.

    config =
        [ Review.Pattern.As.forbid
        ]


## Fail

    view ({ windowSize } as model) =
        Html.text "Hello!"


## Success

    view model =
        let
            { windowSize } =
                model
        in
        Html.text "Hello!"

-}
forbid : Rule
forbid =
    -- TODO collect single variants in dependencies
    Rule.newProjectRuleSchema "Review.Pattern.As.forbid" initialContext
        |> Rule.withModuleVisitor
            (\moduleSchema ->
                moduleSchema
                    |> Rule.withDeclarationListVisitor (\declarationList context -> ( [], declarationList |> List.foldl visitDeclarationForSingleVariant context ))
                    |> Rule.withExpressionEnterVisitor (\expressionNode context -> ( expressionVisitor expressionNode context, context ))
                    |> Rule.withDeclarationEnterVisitor (\declarationNode context -> ( visitDeclaration declarationNode context, context ))
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = projectContextToModule
            , fromModuleToProject = moduleContextToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { singleVariants : Set ( ModuleName, String )
    }


type alias ModuleContext =
    { moduleName : ModuleName
    , moduleNameLookup : ModuleNameLookupTable
    , importSingleVariants : Set ( ModuleName, String )
    , moduleSingleVariants : Set String
    }


moduleContextSingleVariants : ModuleContext -> Set ( ModuleName, String )
moduleContextSingleVariants =
    \context ->
        Set.union
            (context.moduleSingleVariants |> Set.map (\name -> ( context.moduleName, name )))
            context.importSingleVariants


initialContext : Rule.ContextCreator () ProjectContext
initialContext =
    Rule.initContextCreator
        (\() ->
            { singleVariants = Set.empty }
        )


moduleContextToProject : Rule.ContextCreator ModuleContext ProjectContext
moduleContextToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { singleVariants = moduleContext |> moduleContextSingleVariants }
        )


projectContextToModule : Rule.ContextCreator ProjectContext ModuleContext
projectContextToModule =
    Rule.initContextCreator
        (\moduleName moduleNameLookup projectContext ->
            { moduleName = moduleName
            , moduleNameLookup = moduleNameLookup
            , importSingleVariants = projectContext.singleVariants
            , moduleSingleVariants = Set.empty
            }
        )
        |> Rule.withModuleName
        |> Rule.withModuleNameLookupTable


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts context0 context1 =
    Debug.todo ""


visitDeclaration : Node Declaration -> ModuleContext -> List (Rule.Error {})
visitDeclaration (Node _ declaration) context =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            (Elm.Syntax.Node.value functionDeclaration.declaration).arguments
                |> List.concatMap
                    (checkPattern
                        { intoExpression = (Elm.Syntax.Node.value functionDeclaration.declaration).expression
                        , moduleSingleVariants = context.moduleSingleVariants
                        , importSingleVariants = context.importSingleVariants
                        , moduleNameLookup = context.moduleNameLookup
                        }
                    )

        _ ->
            []


visitDeclarationForSingleVariant : Node Declaration -> ModuleContext -> ModuleContext
visitDeclarationForSingleVariant (Node _ declaration) context =
    case declaration of
        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            case choiceTypeDeclaration.constructors of
                (Node _ singleVariant) :: [] ->
                    { context
                        | moduleSingleVariants =
                            context.moduleSingleVariants |> Set.insert (Elm.Syntax.Node.value singleVariant.name)
                    }

                _ ->
                    context


expressionVisitor : Node Expression -> ModuleContext -> List (Rule.Error {})
expressionVisitor expression context =
    case Elm.Syntax.Node.value expression of
        Elm.Syntax.Expression.CaseExpression caseBlock ->
            List.concatMap
                (\( casePatternNode, caseExpression ) ->
                    casePatternNode
                        |> checkPattern
                            { intoExpression = caseExpression
                            , moduleSingleVariants = context.moduleSingleVariants
                            , importSingleVariants = context.importSingleVariants
                            , moduleNameLookup = context.moduleNameLookup
                            }
                )
                caseBlock.cases

        Elm.Syntax.Expression.LetExpression letBlock ->
            List.concatMap
                (\(Node _ letDeclaration) ->
                    case letDeclaration of
                        Elm.Syntax.Expression.LetFunction letFunctionOrValueDeclaration ->
                            List.concatMap
                                (checkPattern
                                    { intoExpression = (Elm.Syntax.Node.value letFunctionOrValueDeclaration.declaration).expression
                                    , moduleSingleVariants = context.moduleSingleVariants
                                    , importSingleVariants = context.importSingleVariants
                                    , moduleNameLookup = context.moduleNameLookup
                                    }
                                )
                                (Elm.Syntax.Node.value letFunctionOrValueDeclaration.declaration).arguments

                        _ ->
                            []
                )
                letBlock.declarations

        _ ->
            []


checkPattern :
    { intoExpression : Node Expression
    , importSingleVariants : Set ( ModuleName, String )
    , moduleSingleVariants : Set String
    , moduleNameLookup : ModuleNameLookupTable
    }
    -> Node Pattern
    -> List (Rule.Error {})
checkPattern config patternNode =
    case patternNode of
        Node patternRange (Elm.Syntax.Pattern.AsPattern patternInAs (Node variableRange variable)) ->
            case patternInAs |> patternKind config of
                Destructuring ->
                    [ Rule.errorWithFix
                        { message = "as pattern is forbidden"
                        , details =
                            [ "You can replace this ... as " ++ variable ++ " pattern by a let."
                            , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern-as/latest."
                            ]
                        }
                        variableRange
                        [ Review.Fix.replaceRangeBy patternRange variable
                        , Debug.todo "insert let at intoExpression"
                        ]
                    ]

        Node _ nonAsPattern ->
            nonAsPattern |> ElmPattern.Extra.sub |> List.concatMap (checkPattern config)


type PatternKind
    = Destructuring
    | Narrowing


patternKind :
    { context
        | importSingleVariants : Set ( ModuleName, String )
        , moduleSingleVariants : Set String
        , moduleNameLookup : ModuleNameLookupTable
    }
    -> Node Pattern
    -> PatternKind
patternKind context =
    \(Node patternRange pattern) ->
        case pattern of
            Elm.Syntax.Pattern.ListPattern _ ->
                Narrowing

            Elm.Syntax.Pattern.TuplePattern partPatterns ->
                if partPatterns |> List.any (\partPattern -> (partPattern |> patternKind context) == Narrowing) then
                    Narrowing

                else
                    Destructuring

            Elm.Syntax.Pattern.RecordPattern patterns ->
                Destructuring

            Elm.Syntax.Pattern.NamedPattern variantRef patterns ->
                case Review.ModuleNameLookupTable.moduleNameAt context.moduleNameLookup patternRange of
                    Nothing ->
                        Narrowing

                    Just [] ->
                        if context.moduleSingleVariants |> Set.member variantRef.name then
                            Destructuring

                        else
                            Narrowing

                    Just (modulePart0 :: modulePart1Up) ->
                        if context.importSingleVariants |> Set.member ( modulePart0 :: modulePart1Up, variantRef.name ) then
                            Destructuring

                        else
                            Narrowing

            Elm.Syntax.Pattern.UnConsPattern _ _ ->
                Narrowing

            Elm.Syntax.Pattern.AsPattern patternInAs name ->
                patternInAs |> patternKind context

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                inParens |> patternKind context

            Elm.Syntax.Pattern.VarPattern name ->
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
