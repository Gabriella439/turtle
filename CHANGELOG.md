1.6.0

* BREAKING CHANGE: Switch to the `FilePath` type from `base` instead of
  `system-filepath`
  * This is a breaking change for a couple of reasons:
    * The `FilePath` type has changed, so the API is not backwards-compatible
      * The thing most likely to break is if you directly imported utilities
        from the `system-filepath` or `system-fileio` packages to operate on
        `turtle`'s `FilePath`s
        * If that happens, you should first check if the `Turtle` module
          exports a utility of the same name.  If so, then switch to that
        * If there is no equivalent substitute from the `Turtle` module then
          you will have to change your code to use the closest equivalent
          utility from the `filepath` or `directory` package
        * If you were previously using any of the `system-filepath` or
          `system-fileio` utilities re-exported from the `Turtle` module then
          those utilities will not break as they have been replaced with
          versions compatible with the `FilePath` type from `base`
      * The second thing most likely to break is any code that relies on
        typeclasses since because if you defined any instances for the
        `FilePath` type exported by `turtle` then those instances will now
        overlap with any instances defined for the `String` type
      * The conversion utilities (e.g. `toText`, `encodeString`) will still
        work, so code that used those conversion utilities should be less
        affected by this change
    * The behavior of the `collapse` utility is subtly different
      * `collapse` no longer interprets `..` in paths
      * This new behavior is more correct in the presence of symlinks, so the
        change is (hopefully) an improvement to downstream code
  * The new API strives to match the old behavior as closely as possible
    * … so this should (hopefully) not break too much code in practice
    * With the exception of the `collapse` function the new API should be
      bug-for-bug compatible with the old API
      * Most of the surprising behavior inherited from the old API is around
        how `.` and `..` are handled in paths
        * `parent ".." == "."` is an example of such surprising behavior
    * At some point in the future we may fix bugs in these utilities inherited
      from `system-filepath` / `system-fileio`, but no decision either way has
      been made, yet
  * Some old utilities are marked `DEPRECATED` if their behavior exactly matches
    the behavior of an existing utility from the `filepath` or `directory`
    package
    * These may be eventually removed at some point in the future or they
      remain in a deprecated state indefinitely.  No decision either way has
      been made
    * The `Turtle` module also re-exports any utility suggested by a
      `DEPRECATED` pragma as a convenience
  * Other utilities are not deprecated if the old behavior significantly departs
    from any existing utility from the `filepath` or `directory` package
    * For example, the behavior of the `filename` utility differs from the
      behavior of `System.FilePath.takeFileName` for filenames that begin with a
      `.`, so we have to preserve the old behavior to avoid breaking downstream
      code
    * At some point in the future utilities like these may be deprecated in
      favor of their closest analogs in the `filepath` / `directory` packages or
      they may be supported indefinitely.  No decision either way has been made
  * If you want to try to author code that is compatible with both the
    pre-1.6 and post-1.6 API:
    * If you add any instances to the `FilePath` type, import it qualified
      directly from the `system-filepath` package and use it only for instances
    * Otherwise, don't import anything else from the `system-filepath` /
      `system-fileio` packages if you can help it.  Instead, restrict yourself
      entirely to the utilities and `FilePath` type exported by the `Turtle`
      module
    * Use the conversion utilities (e.g. `encodeStrings`, even if they are not
      necessary post-1.6)
    * If that's still not enough, use `CPP` and good luck!

1.5.25

* Build against latest version of `Win32` package

1.5.24

* Expose `Format` constructor

1.5.23

* Add `fromIO` utility
* Build against GHC 9.0 / 9.2

1.5.22

* Add new `update` utility
* Improve documentation for `limit`

1.5.21

* Build against `optparse-applicative-0.16.0.0`

1.5.20

* Build against `doctest-0.17`
* Only depend on `semigroups` for GHC < 8.0

1.5.19

* Add pattern synonyms for `Size`

1.5.18

* Fix space leak

1.5.17

* Add `optionsExt`: Extended version of `options` with header, footer,
  porgram-description and version information in `--help` flag
* Add `readlink`

1.5.16

* Add `cptreeL`

1.5.15

* Add `toLines`
* Add `Turtle.Bytes.{fromUTF8,toUTF8}`
* Add `Turtle.Bytes.{compress,decompress}`
* Always expose a `MonadFail` instance, relying on the `fail` package
  where needed. Related GHC 8.8 preparedness.

1.5.14

* Fix `cptree` to copy symlinks instead of descending into them
    * See: https://github.com/Gabriel439/Haskell-Turtle-Library/pull/344
* Build against newer versions of `Win32` package

1.5.13

* Fix `chmod` bug
    * See: https://github.com/Gabriel439/Haskell-Turtle-Library/pull/337
* Add `reduce` and re-export `(<&>)`
    * See: https://github.com/Gabriel439/Haskell-Turtle-Library/pull/332

1.5.12

* Increase upper bound on `containers`

1.5.11

* Don't forward broken pipe exceptions when using `inproc`
    * See: https://github.com/Gabriel439/Haskell-Turtle-Library/pull/321
* Increase upper bound on `stm`
    * See: https://github.com/Gabriel439/Haskell-Turtle-Library/pull/321
* Tutorial improvements:
    * See: https://github.com/Gabriel439/Haskell-Turtle-Library/pull/322

1.5.10

* Increase upper bound on `doctest` and `criterion`

1.5.9

* Add `symlink`

1.5.8

* Bug fix: `invert` no longer rejects inputs where a prefix matches the inverted
  pattern
    * See: https://github.com/Gabriel439/Haskell-Turtle-Library/pull/297
* Add lsdepth, findtree, cmin, and cmax
* Increase upper bound on `temporary` and `foldl`

1.5.7

* Increase upper bound on `doctest`

1.5.6

* Increase upper bound on `exceptions`

1.5.5

* Increase upper bound on `criterion`

1.5.4

* Increase upper bound on `exceptions`

1.5.3

* Increase upper bound on `doctest`

1.5.2

* Increase upper bound on `async`

1.5.1

* GHC 8.4 support
* Re-export `encodeString`/`decodeString`
* Update tutorial to use `stack script`
* Increase upper bounds on dependencies

1.5.0

* BREAKING CHANGE: Add `MonadCatch` instance for `Shell`
    * This requires a breaking change to the internal implementation of `Shell`
    * Most breaking changes can be fixed by replacing the `Shell` constructor
      with the newly added `_Shell` utility for ease of migration
    * If you don't use the `Shell` constructor then this change likely does not
      affect you
* Add `eprintf`

1.4.5

* Add `grepText`, `uniq`, `nub`, `sort` to `Turtle.Prelude`
* Increase upper bound on `unix-compat`

1.4.4

* Fix small mistake in tutorial

1.4.3

* Increase upper bound on `doctest`

1.4.2

* Add `sed{Prefix,Suffix,Entire}` and `inplace{Prefix,Suffix,Entire}`

1.4.1

* Increase upper bound on `doctest`

1.4.0

* BREAKING CHANGE: Remove unnecessary `Maybe` from type of `single`
* BREAKING CHANGE: Consolidate `searchable` and `executable`
* `stream{,WithErr}` now throws an `ExitCode` on failure

1.3.6

* Build against `ghc-8.2`
* Relax upper bound on `optparse-applicative` and `foldl`

1.3.5

* Increase upper bound on `foldl`

1.3.4

* Bug fix: `cptree` now correctly copies files instead of creating directories
  of the same name
* Increase upper bound on `criterion`

1.3.3

* Bug fix: Change `textToLines` to behave like `Data.Text.splitOn "\n"`
   instead of `Data.Text.unlines`
    * This fixes weird behavior around handling empty strings.  `splitOn` does
      the right thing, but `unlines` does not.  For example, this indirectly
      fixes a regression in `sed`, which would discard empty lines
* Bug fix: `which`/`whichAll` now behave correctly on Windows
* Add new `cptree`/`single` utilities
* Documentation fixes

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
