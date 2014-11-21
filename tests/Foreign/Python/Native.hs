{-# LINE 1 "tests/Foreign/Python/Native.hsc" #-}
-- Copyright (c) 2014 Sebastian Wiesner <swiesner@lunaryorn.com>
{-# LINE 2 "tests/Foreign/Python/Native.hsc" #-}

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}

module Foreign.Python.Native where


{-# LINE 28 "tests/Foreign/Python/Native.hsc" #-}

import Data.Int
import Foreign.C (CInt(..),CString)
import Foreign.Ptr (Ptr,FunPtr)

data RawPyObject

type RawPyObjectHandle = Ptr RawPyObject
type PySSizeT = Int64
{-# LINE 37 "tests/Foreign/Python/Native.hsc" #-}

foreign import ccall unsafe "python2.7/Python.h &Py_DecRef"
  pyDecRef :: FunPtr (RawPyObjectHandle -> IO ())

foreign import ccall unsafe "python2.7/Python.h Py_IncRef"
  pyIncRef :: RawPyObjectHandle -> IO ()

foreign import ccall safe "python2.7/Python.h PyImport_ImportModule"
  pyImport_ImportModule :: CString -> IO RawPyObjectHandle

foreign import ccall safe "python2.7/Python.h Py_InitializeEx"
  pyInitializeEx :: CInt -> IO ()

foreign import ccall unsafe "python2.7/Python.h PyString_FromStringAndSize"
  pyString_FromStringAndSize :: CString -> PySSizeT -> IO RawPyObjectHandle

foreign import ccall unsafe "python2.7/Python.h PyString_AsStringAndSize"
  pyString_AsStringAndSize :: RawPyObjectHandle -> Ptr CString -> Ptr PySSizeT -> IO CInt

foreign import capi unsafe "python2.7/Python.h PyUnicode_AsUTF8String"
  pyUnicode_AsUTF8String :: RawPyObjectHandle -> IO RawPyObjectHandle

foreign import capi unsafe "python2.7/Python.h PyUnicode_FromStringAndSize"
  pyUnicode_FromStringAndSize :: CString -> PySSizeT -> IO RawPyObjectHandle

foreign import ccall unsafe "python2.7/Python.h PyTuple_New"
  pyTuple_New :: PySSizeT -> IO RawPyObjectHandle

foreign import ccall unsafe "python2.7/Python.h PyTuple_SetItem"
  pyTuple_SetItem :: RawPyObjectHandle -> PySSizeT -> RawPyObjectHandle -> IO CInt

foreign import ccall unsafe "python2.7/Python.h PyDict_New"
  pyDict_New :: IO RawPyObjectHandle

foreign import ccall unsafe "python2.7/Python.h PyDict_SetItem"
  pyDict_SetItem :: RawPyObjectHandle -> RawPyObjectHandle -> RawPyObjectHandle -> IO CInt

foreign import ccall unsafe "python2.7/Python.h PyObject_Call"
  pyObject_Call :: RawPyObjectHandle -> RawPyObjectHandle -> RawPyObjectHandle -> IO RawPyObjectHandle

foreign import ccall unsafe "python2.7/Python.h PyObject_GetAttrString"
  pyObject_GetAttrString :: RawPyObjectHandle -> CString -> IO RawPyObjectHandle

foreign import ccall unsafe "python2.7/Python.h PyObject_Unicode"
  pyObject_Unicode :: RawPyObjectHandle -> IO RawPyObjectHandle

foreign import ccall unsafe "python2.7/Python.h PyErr_Fetch"
  pyErr_Fetch :: Ptr RawPyObjectHandle -> Ptr RawPyObjectHandle -> Ptr RawPyObjectHandle -> IO ()

foreign import ccall unsafe "python2.7/Python.h PyErr_NormalizeException"
  pyErr_NormalizeException :: Ptr RawPyObjectHandle
                           -> Ptr RawPyObjectHandle
                           -> Ptr RawPyObjectHandle
                           -> IO ()
