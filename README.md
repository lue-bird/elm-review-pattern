# [elm-review-pattern](https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/1.0.2/)

[`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules
- [`Review.Pattern.As.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/1.0.2/Review-Pattern-As/#forbid): Forbid `... as ..variable..` patterns.
- [`Review.Pattern.Record.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/1.0.2/Review-Pattern-Record/#forbid): Forbid `{ field, ... }` record patterns.

Check their module documentation to find out why you might want to enable these rules.

## try

You can try the example configuration below out using

```bash
elm-review --template lue-bird/elm-review-pattern/example
```
which will run both rules. Add `--rules ...` to select only one.

## configure

```elm
module ReviewConfig exposing (config)

import Review.Pattern.As
import Review.Pattern.Record
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.Pattern.Record.forbid
    , Review.Pattern.As.forbid
    ]
```
