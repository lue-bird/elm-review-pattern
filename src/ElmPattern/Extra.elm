module ElmPattern.Extra exposing (sub)

import Elm.Syntax.Node
import Elm.Syntax.Pattern


{-| All surface-level child patterns (not recursive).
-}
sub : Elm.Syntax.Pattern.Pattern -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
sub pattern =
    case pattern of
        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns

        Elm.Syntax.Pattern.RecordPattern patterns ->
            patterns |> List.map (Elm.Syntax.Node.map Elm.Syntax.Pattern.VarPattern)

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            [ headPattern, tailPattern ]

        Elm.Syntax.Pattern.AsPattern pattern_ name ->
            [ name |> Elm.Syntax.Node.map Elm.Syntax.Pattern.VarPattern, pattern_ ]

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            [ inParens ]

        Elm.Syntax.Pattern.VarPattern _ ->
            []

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []
