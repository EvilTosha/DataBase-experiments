#!/bin/sh

screen -dmS database-web-console sbcl --lose-on-corruption --disable-ldb --load start.lisp
