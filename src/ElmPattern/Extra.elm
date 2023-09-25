module ElmPattern.Extra exposing (sub)

import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))


{-| All surface-level child patterns (not recursive).
-}
sub : Pattern -> List (Node Pattern)
sub pattern =
    case pattern of
        ListPattern patterns ->
            patterns

        TuplePattern patterns ->
            patterns

        RecordPattern patterns ->
            patterns |> List.map (Elm.Syntax.Node.map Elm.Syntax.Pattern.VarPattern)

        NamedPattern _ patterns ->
            patterns

        UnConsPattern headPattern tailPattern ->
            [ headPattern, tailPattern ]

        AsPattern pattern_ name ->
            [ name |> Elm.Syntax.Node.map Elm.Syntax.Pattern.VarPattern, pattern_ ]

        ParenthesizedPattern inParens ->
            [ inParens ]

        VarPattern _ ->
            []

        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []
