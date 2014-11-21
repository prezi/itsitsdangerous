itsitsdangerous
===============

Minimal Haskell clone of Python's `itsdangerous` library.

## Python bindings and testing

### FFI
This lib is tested aginst original library through FFI bindings. Python FFI which is underneath is entirely copied from Sebastian Wiesner repository:

https://github.com/lunaryorn/blog/tree/master/src/Foreign

and here is related and really nice blogpost by the same Author:

http://www.lunaryorn.com/2014/04/15/calling-python-from-haskell.html

### FFI
To run tests:

    cabal install --enable-tests
    cabal test
