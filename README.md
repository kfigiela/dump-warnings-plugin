# dump-warnings-plugin

Collects warnings emitted by GHC to files, so they can be inspected later.

## Motivation

In a typical cabal/stack scenario incremental builds happen during development. Once module is built successfully, the toolchain will not rebuild it unless it (or it's dependencies) change. This effectively means that warnings are lost and one can only see them again by running a full rebuild (e.g. `cabal clean && cabal build`).

Many projects implement a "no warnings" policy as a part of good practice. This is typically implemented by enabling `-Werror` GHC option for CI builds, so that warnings do not leak into production branches. On the other hand, local developments environment are not likely to have `-Werror` enabled as this is not practical. Overall, warnings that are not emitted cause either need to do a manual check/cleanup cycles (e.g. `cabal clean && cabal build`) before pushing to the repo, or they cause CI jobs failures. Either of those, it slows down software development cycle.

## Solution

This GHC plugin collects the warnings as a JSONL file placed next to `*.hi` or `*.o` files. The files are persistent, so the warnings can be read without the need to do a full build cycle.

At the moment, one can use a shell one-liner based on `find` and `jq` to pretty-print a warning report:
```shell
find dist-newstyle/ -name "*.warn" -exec cat {} \; | jq -r '.absFile + ":" + (.location.startLine | tostring) + ":" + (.location.startCol | tostring) + " " + .severity + "/" + .flag + "\n" + .message + "\n"'
```

## Limitations

* Some warnings are not collected (e.g. `-Wunused-packages`). GHC does not seem to call log hook for these

## TODO

* [ ] Test with GHC 9.6
* [ ] Enable CI builds
* [ ] Add some tests
* [ ] Add executable to collect warnings instead of relying on shell scripts
* [ ] Check if path handling is correct for non-typical scenarios
