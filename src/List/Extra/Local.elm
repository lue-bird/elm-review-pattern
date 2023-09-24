module List.Extra.Local exposing (firstJustMap)


firstJustMap : (element -> Maybe found) -> List element -> Maybe found
firstJustMap tryMap list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case tryMap head of
                Just found ->
                    Just found

                Nothing ->
                    firstJustMap tryMap tail
