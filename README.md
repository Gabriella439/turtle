# Turtle v1.4.4

Turtle is a reimplementation of the Unix command line environment in Haskell so
that you can use Haskell as a scripting language or a shell.  Think of `turtle`
as `coreutils` embedded within the Haskell language.

## Quick start

* Install [Stack](https://github.com/commercialhaskell/stack)

```
$ stack ghci turtle
Prelude> :set -XOverloadedStrings
Prelude> import Turtle
```

... and try out some basic filesystem operations:

```
Prelude Turtle> cd "/tmp"
Prelude Turtle> mkdir "test"
Prelude Turtle> touch "test/foo"
Prelude Turtle> testfile "test/foo"
True
Prelude Turtle> rm "test/foo"
Prelude Turtle> testfile "test/foo"
False
Prelude Turtle> rmdir "test"
Prelude Turtle> view (lstree "/usr/lib")
FilePath "/usr/lib/gnome-screensaver"
FilePath "/usr/lib/gnome-screensaver/gnome-screensaver-dialog"
FilePath "/usr/lib/libplist.so.1.1.8"
FilePath "/usr/lib/tracker"
FilePath "/usr/lib/tracker/tracker-miner-fs"
FilePath "/usr/lib/tracker/tracker-extract"
FilePath "/usr/lib/tracker/tracker-writeback"
FilePath "/usr/lib/tracker/tracker-search-bar"
FilePath "/usr/lib/tracker/tracker-store"
FilePath "/usr/lib/libgif.so.4.1"
...
```

To learn more, read the [turtle tutorial](https://hackage.haskell.org/package/turtle/docs/Turtle-Tutorial.html).

## Goals

The `turtle` library focuses on being a "better Bash" by providing a typed and
light-weight shell scripting experience embedded within the Haskell language.
If you have a large shell script that is difficult to maintain, consider
translating it to a "`turtle` script" (i.e. a Haskell script using the `turtle`
library).

Among typed languages, Haskell possesses a unique combination of features that
greatly assist scripting:

* Haskell has global type inference, so all type annotations are optional
* Haskell is functional and not object-oriented, so boilerplate is minimal
* Haskell can be type-checked and interpreted quickly (< 1 second startup time)

## Features

* *Batteries included:* Command an extended suite of predefined utilities

* *Interoperability:* You can still run external shell commands

* *Portability:* Works on Windows, OS X, and Linux

* *Exception safety:* Safely acquire and release resources 

* *Streaming:* Transform or fold command output in constant space

* *Patterns:* Use typed regular expressions that can parse structured values

* *Formatting:* Type-safe `printf`-style text formatting

* *Modern:* Supports `text` and `system-filepath`

## Development Status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-Turtle-Library.png)](https://travis-ci.org/Gabriel439/Haskell-Turtle-Library)

`turtle`'s types and idioms are reasonably complete and I don't expect there
to be significant changes to the library's core API.  The only major
functionality that I might add in the future would be to wrap
`optparse-applicative` in a simpler API.

The set of available tools currently covers as many filesystem utilities as I
could find across Hackage, but I would like to continue to add to the set of
available tools to minimally match `coreutils`.

## Community Resources

* The
  [haskell-turtle tag](http://stackoverflow.com/questions/tagged/haskell-turtle)
  on Stack Overflow

## How to contribute

* Contribute more utilities

* Write `turtle` tutorials

## License (BSD 3-clause)

Copyright (c) 2017 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of Gabriel Gonzalez nor the names of other contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
