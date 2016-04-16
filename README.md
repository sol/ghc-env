# ghc-env: Automatically download and manage multiple versions of GHC

## Usage
```
$ ghc-env 7.10.3 ghci            # run `ghci` 7.10.3
$ ghc-env 7.10.3 bash            # run `bash` with `ghc` 7.10.3 in your PATH
$ eval `ghc-env 7.10.3 --env`    # change current shell's PATH to include `ghc` 7.10.3 in PATH
$ ghc-env 7.10.3                 # open your default shell with `ghc` 7.10.3 in your PATH
$ ghc-env                        # open your default shell with a default `ghc` in your PATH
```

## Implementation

`ghc-env` reuses code from `stack` to install `ghc`.  The following file
contains a list of available versions:
<https://raw.githubusercontent.com/fpco/stackage-content/master/stack/stack-setup-2.yaml>
