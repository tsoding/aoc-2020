# [Day 5](https://adventofcode.com/2020/day/5) solution in [C++](https://isocpp.org/)

## Screencast

[![screencast](http://i3.ytimg.com/vi/vVjNyY2jkqg/hqdefault.jpg)](https://www.youtube.com/watch?v=vVjNyY2jkqg&list=PLpM-Dvs8t0Vba3v-9lweHuomr0DPhdX6P&t=829s)

## Tested on

```console
$ clang++ --version
clang version 7.0.1-8+deb10u2 (tags/RELEASE_701/final)
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/bin
$ g++ --version
g++ (Debian 8.3.0-6) 8.3.0
Copyright (C) 2018 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

## Expected Result

```console
Input file: input.txt
Part 1:
  ...
  Answer: 855
Part 2:
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  . * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  * * * * * * * *
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
  . . . . . . . .
```

## Quick Start

- install [GCC] or [Clang] and make sure `g++` or `clang++` are available in `$PATH`
- `$ CXX=g++ make` in case of [GCC]
- `$ CXX=clang++ make` in case of [Clang]

[GCC]: https://gcc.gnu.org/
[Clang]: https://clang.llvm.org/
