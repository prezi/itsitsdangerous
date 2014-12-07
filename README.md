itsitsdangerous
===============

Minimal Haskell clone of Python's `itsdangerous` library. It allows you to easily interact with services signed with original library.

## Python bindings and testing

### FFI
This lib is tested aginst original library through FFI bindings. Python FFI which is underneath is entirely copied from Sebastian Wiesner repository:

https://github.com/lunaryorn/blog/tree/master/src/Foreign

and here is related and really nice blogpost by the same Author:

http://www.lunaryorn.com/2014/04/15/calling-python-from-haskell.html

### Testing
To run tests you have to install original, python itsdangerous library and then run:

    cabal install --enable-tests
    cabal test
