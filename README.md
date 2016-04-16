# Riff
## Synopsis

Riff is a command line application to rename files in a sane way on Unix systems. It does this by replacing "invalid" characters with an underscore.

## Installing

Riff is written in Haskell and can therefore most easily be built using stack. To do this obtain the latest release from https://github.com/commercialhaskell/stack/releases and built/install with `stack build --copy-bins`.

## Usage Example

Help can be obtained by running `riff --help`. To view the list of characters that are considered "valid" run `riff --validchars`.

Typical usage might include recursing directories and lowercasing filenames. This can be done by running riff as follows: `riff --lower --recurse /path/to/files`

It is possible to perform a dryrun before letting riff loose on your files with the `--dryrun` switch.

## Motivation

Riff started because I hated spaces in my filenames and wanted to get rid of them. From there it evolved to get rid of all the other annoyances in filenames like upper case characters and punctuation.

## Tests

The test suite, while admittedly not very comprehensive currently, can be run with `stack test`.

## Contributors

Feel free to contribute by sending pull requests.

## License

Riff is licensed under a BSD3-style license.
