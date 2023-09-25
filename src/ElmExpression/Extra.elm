module ElmExpression.Extra exposing (sub)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))


{-| Get all immediate child expressions of an expression
-}
sub : Expression -> List (Node Expression)
sub expression =
    case expression of
        Elm.Syntax.Expression.LetExpression letBlock ->
            letBlock.declarations
                |> List.map Elm.Syntax.Node.value
                |> List.map
                    (\letDeclaration ->
                        case letDeclaration of
                            Elm.Syntax.Expression.LetFunction letFunction ->
                                letFunction.declaration |> Elm.Syntax.Node.value |> .expression

                            Elm.Syntax.Expression.LetDestructuring _ expression_ ->
                                expression_
                    )
                |> (::) letBlock.expression

        Elm.Syntax.Expression.ListExpr expressions ->
            expressions

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.RecordExpr fields ->
            fields |> List.map (\(Node _ ( _, value )) -> value)

        Elm.Syntax.Expression.RecordUpdateExpression _ updaters ->
            updaters |> List.map (\(Node _ ( _, newValue )) -> newValue)

        Elm.Syntax.Expression.RecordAccess recordToAccess _ ->
            [ recordToAccess ]

        Elm.Syntax.Expression.Application applicationElements ->
            applicationElements

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases |> List.map (\( _, caseExpression ) -> caseExpression))

        Elm.Syntax.Expression.OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        Elm.Syntax.Expression.IfBlock condition then_ else_ ->
            [ condition, then_, else_ ]

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.ParenthesizedExpression expressionInParens ->
            [ expressionInParens ]

        Elm.Syntax.Expression.Negation expressionInNegation ->
            [ expressionInNegation ]

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.PrefixOperator _ ->
            []
