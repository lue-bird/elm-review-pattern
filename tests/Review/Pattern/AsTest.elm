module Review.Pattern.AsTest exposing (all)

import Review.Pattern.As
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Pattern.As.forbid"
        [ test "should not report an error when using let"
            (\() ->
                """module A exposing (..)
a record =
    let
        { field } =
            record
    in
    f field
"""
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectNoErrors
            )
        , test "should report an error in function declaration argument"
            (\() ->
                """module A exposing (..)
a ({ field } as record) =
    f field
"""
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "as pattern is forbidden"
                            , details =
                                [ "You can replace this ... as record pattern by a let."
                                , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                                ]
                            , under = "record"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a (record) =
    let
        { field } =
            record
    in
    f field
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
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "as pattern is forbidden"
                            , details =
                                [ "You can replace this ... as record pattern by a let."
                                , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                                ]
                            , under = "record"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    let
        b (record) =
            let
                { field } =
                    record
            in
            f field
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
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "as pattern is forbidden"
                            , details =
                                [ "You can replace this ... as record pattern by a let."
                                , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                                ]
                            , under = "record"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    \\(record) ->
        let
            { field } =
                record
        in
        f field
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
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "as pattern is forbidden"
                            , details =
                                [ "You can replace this ... as record pattern by a let."
                                , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                                ]
                            , under = "record"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    let
        (record) =
            x
    in
    let
        { field } =
            record
    in
    f field
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
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "as pattern is forbidden"
                            , details =
                                [ "You can replace this ... as record pattern by a let."
                                , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                                ]
                            , under = "record"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a maybe =
    case maybe of
        Nothing ->
            Nothing

        Just (record) ->
            let
                { field } =
                    record
            in
            f field
"""
                        ]
            )
        , test "should report an error in destructuring part of case with module single-variant"
            (\() ->
                """module A exposing (..)
type Single a
    = Single a
a maybe =
    case maybe of
        Nothing ->
            Nothing

        Just ((Single { field }) as record) ->
            f field
"""
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "as pattern is forbidden"
                            , details =
                                [ "You can replace this ... as record pattern by a let."
                                , "Using `as` is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                                ]
                            , under = "record"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
type Single a
    = Single a
a maybe =
    case maybe of
        Nothing ->
            Nothing

        Just (record) ->
            let
                Single { field } =
                    record
            in
            f field
"""
                        ]
            )
        , test "should report an error in narrowing part of case"
            (\() ->
                """module A exposing (..)
a =
    case computeMaybe x of
        Nothing ->
            Nothing

        (Just { field }) as maybe ->
            f field
"""
                    |> Review.Test.run Review.Pattern.As.forbid
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "as pattern is forbidden"
                            , details =
                                [ "The variable introduced in this ... as maybe pattern has a broader type than the case allows. It's best to convert the narrow values into a broad type as late as possible or not at all."
                                , "Using `as` in general is often less readable and tricky to use well, see the rule's readme: https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/latest."
                                ]
                            , under = "maybe"
                            }
                        ]
            )
        ]
