# elm-review-pattern-as

[`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule [`Review.Pattern.As.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-pattern-as/1.0.0/Review-Pattern-As#forbid): Forbid `... as ..variable..` patterns.

## why

  - `as` can quickly lead to barely readable, long patterns:
    ```elm
    Node _ (Expression.Lambda ({ argumentPatterns, resultExpression, arrowRange } as lambda)) ->
        x
    ```
    compared to
    ```elm
    Node _ (Expression.Lambda lambda) ->
        let
            { argumentPatterns, resultExpression, arrowRange } =
                lambda
        in
        x
    ```
  - less syntax to learn to use right and keep in your head
  - `as` in `case of` patterns can create variables with types that know less than you:
    ```elm
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
    ```
    compared to
    ```elm
    highestScoreUi ... =
        case computeScores ... of
            [] ->
                textUi "no scores, yet"

            scoresHead :: scoresTail ->
                scoreUi (List.Nonempty.maximum ( scoresHead, scoresTail ))

    ```
    There are many more reasons for not introducing variables like that, see [`VariablesBetweenCaseOf.AccessInCases.forbid`](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-variables-between-case-of-access-in-cases/latest/#why).

## try

You can try the example configuration above out by running the following command:

```bash
elm-review --template lue-bird/elm-review-pattern-as/example
```

## configure

```elm
module ReviewConfig exposing (config)

import Review.Pattern.As
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.Pattern.As.forbid
    ]
```

