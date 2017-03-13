1.3.2

* Fix bugs in subprocess management
* Generalize type of `repr` to return any type that implements `IsString`
* Add `optLine`, `argLine`, and `l` utilities to simplify working with `Line`s

1.3.1

* `find` no longer follows symlinks
* Increase upper bound on `directory`

1.3

* BREAKING CHANGE: Several utilities now produce and consume `Line`s instead of
  `Text`
    * The purpose of this change is to fix a very common source of confusion for
      new users about which utilities are line-aware
    * Most of the impact on existing code is just changing the types by
      replacing `Text` with `Line` in the right places.  The change at the
      term level should be small (based on the changes to the tutorial examples)
* BREAKING CHANGE: `Description` now wraps a `Doc` instead of `Text`
    * In the most common case where users use string literals this has no effect
* New `Turtle.Bytes` module that provides `ByteString` variations on subprocess
  runners
* Fix `du` reporting incorrect sizes for directories
* Add `pushd`, `stat`, `lstat`, `which`, `procStrictWithErr`,
  `shellStrictWithErr`, `onFiles`, `header`, `subcommandGroup`, and `parallel`
* Backport `need` to GHC 7.6.3
* Fix missing help text for option parsers
* Fix bugs in subprocess management

1.2.8

* Increase upper bound on `time` and `transformers`
* Fix incorrect lower bound for `base`

1.2.7

* Increase upper bound on `clock` dependency

1.2.6

* Generalize several types to use `MonadManaged`
* Generalize type of `printf` to use `MonadIO`
* Add `system`, and `copymod`
* Fix `rmtree` to more accurately match behavior of `rm -r`

1.2.5

* Add `printf`, `utc`, `procs`, and `shells`

1.2.4

* Generalize type of `d` format specifier to format any `Integral` type
* Add `inprocWithErr`, `inShellWithErr`, `inplace`, and `sz`

1.2.3

* Add `subcommand` and `testpath`
* Use line buffering for `Text`-based subprocesses

1.2.2

* Re-export `with`
* Add `begins`, `ends`, `contains`, `lowerBounded`, `mktempfile`, `nl`, `paste`
  `endless`, `lsif`, and `cut`
* Fix subprocess management bugs

1.2.1

* Fix subprocess management bugs

1.2.0

* BREAKING CHANGE: `du` now returns a `Size` instead of an `Integer`
* New `Turtle.Options` module that provides convenient utilities for options
  parsing
* Add `hostname`, `outhandle`, `stderr`, `cache`, `countChars`, `countWords`,
  and `countLines`
* Fix subprocess management bugs

1.1.1

* Add `bounded`, `upperBounded`, `procStrict`, `shellStrict`, `arguments`
* Add several `Permissions`-related commands
* Generalize several types to `MonadIO`

1.1.0

* BREAKING CHANGE: Remove `Floating`/`Fractional` instances for `Pattern` and
  `Shell`
* BREAKING CHANGE: Change behavior of `Num` instance for `Pattern` and `Shell`
* Re-export `(&)`
* Add `asciiCI`, `(.||.)`, `(.&&.)`, `strict`

1.0.2

* Add `fp` format specifier
* Add `chars`/`chars` high-efficiency parsing primitives
* Fix bugs in path handling

1.0.1

* Generalize type of `die`
* Fix doctest

1.0.0

* Initial release
