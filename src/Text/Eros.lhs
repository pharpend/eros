|
Module       : Text.Eros
Description  : Capstone Module for eros
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux

This module serves as a bit of a capstone to the whole eros
library. The idea being you can just import this module, and get
all of the functions from all the rest of eros.

You will have to look in the documentation for the sub-modules for
the functions. I haven't quite figured out how to get the
documentation to show up here yet.

> module Text.Eros (module Text.Eros) where

> import Text.Eros.Message    as Text.Eros
> import Text.Eros.Phrase     as Text.Eros
> import Text.Eros.Phraselist as Text.Eros
