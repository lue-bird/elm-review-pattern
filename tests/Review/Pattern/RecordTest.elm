module Review.Pattern.RecordTest exposing (all)

import Review.Pattern.Record
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Pattern.Record"
        [ test "should report but not fix when no `as` is provided"
            (\() ->
                """module A exposing (..)
a { field } =
    f field
"""
                    |> Review.Test.run Review.Pattern.Record.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "record pattern is forbidden"
                            , details =
                                [ "You can replace this pattern by a variable and convert each use of a destructured field into a record access (`variable.field`). If you add `as yourRecordName`, I can automatically fix this."
                                , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                                ]
                            , under = "{ field }"
                            }
                        ]
            )
        , test "should report but not fix when field is used in record setter"
            (\() ->
                """module A exposing (..)
a ({ field } as record) =
    f { field | x = y }
"""
                    |> Review.Test.run Review.Pattern.Record.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "record pattern is forbidden"
                            , details =
                                [ "You can replace this pattern by a variable and convert each use of a destructured field into a record access (`variable.field`)."
                                , "Right now, I can't provide this as a fix because a field variable is used in a record setter { field | ... } and these don't allow { variable.field | ... }. Maybe use a let for this specific field?"
                                , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                                ]
                            , under = "{ field }"
                            }
                        ]
            )
        , test "should report an error in function declaration argument"
            (\() ->
                """module A exposing (..)
a ({ field } as record) =
    f field
"""
                    |> Review.Test.run Review.Pattern.Record.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "record pattern is forbidden"
                            , details =
                                [ "You can replace this pattern by the variable after `as` and convert each use of a destructured field into a record access (`variable.field`)."
                                , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                                ]
                            , under = "{ field }"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a (record) =
    f record.field
"""
                        ]
            )
        , test "should report an error in let function declaration argument"
            (\() ->
                """module A exposing (..)
a =
    let
        b ({ field } as record) =
            f field
    in
    b x
"""
                    |> Review.Test.run Review.Pattern.Record.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "record pattern is forbidden"
                            , details =
                                [ "You can replace this pattern by the variable after `as` and convert each use of a destructured field into a record access (`variable.field`)."
                                , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                                ]
                            , under = "{ field }"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    let
        b (record) =
            f record.field
    in
    b x
"""
                        ]
            )
        , test "should report an error in lambda argument"
            (\() ->
                """module A exposing (..)
a =
    \\({ field } as record) ->
        f field
"""
                    |> Review.Test.run Review.Pattern.Record.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "record pattern is forbidden"
                            , details =
                                [ "You can replace this pattern by the variable after `as` and convert each use of a destructured field into a record access (`variable.field`)."
                                , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                                ]
                            , under = "{ field }"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    \\(record) ->
        f record.field
"""
                        ]
            )
        , test "should report an error in let destructuring pattern"
            (\() ->
                """module A exposing (..)
a =
    let
        ({ field } as record) =
            x
    in
    f field
"""
                    |> Review.Test.run Review.Pattern.Record.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "record pattern is forbidden"
                            , details =
                                [ "You can replace this pattern by the variable after `as` and convert each use of a destructured field into a record access (`variable.field`)."
                                , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                                ]
                            , under = "{ field }"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    let
        (record) =
            x
    in
    f record.field
"""
                        ]
            )
        , test "should report an error in destructuring part of case"
            (\() ->
                """module A exposing (..)
a maybe =
    case maybe of
        Nothing ->
            Nothing

        Just ({ field } as record) ->
            f field
"""
                    |> Review.Test.run Review.Pattern.Record.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "record pattern is forbidden"
                            , details =
                                [ "You can replace this pattern by the variable after `as` and convert each use of a destructured field into a record access (`variable.field`)."
                                , "Record patterns are usually less readable and tricky to use well, see the rule's documentation: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest/Review-Pattern-Record#why"
                                ]
                            , under = "{ field }"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a maybe =
    case maybe of
        Nothing ->
            Nothing

        Just (record) ->
            f record.field
"""
                        ]
            )
        ]
