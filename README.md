[`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules
  - [`Review.Pattern.As.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/1.0.3/Review-Pattern-As/#forbid): forbid `... as ..variable..` patterns
  - [`Review.Pattern.Record.forbid`](https://package.elm-lang.org/packages/lue-bird/elm-review-pattern/1.0.3/Review-Pattern-Record/#forbid): forbid `{ field, ... }` record patterns

Check their module documentation to find out why you might want to enable these rules.

## try

```bash
elm-review --template lue-bird/elm-review-pattern/example
```
will run both rules. Add `--rules ...` to select only one.

## configure

```elm
module ReviewConfig exposing (config)

import Review.Pattern.As
import Review.Pattern.Record
import Review.Rule

config : List Review.Rule.Rule
config =
    [ Review.Pattern.Record.forbid
    , Review.Pattern.As.forbid
    ]
```
