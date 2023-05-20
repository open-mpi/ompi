.. _label-running-dynamic-loading-libmpi:

Dynamically loading ``libmpi`` at runtime
=========================================

If you want to explicitly load Open MPI's shared library ``libmpi`` at
runtime by using ``dlopen()`` from C/C ++ or something like the
``ctypes`` package from Python, extra care is required.  The
default configuration of Open MPI uses ``dlopen()`` internally to load
its support components.  These components rely on symbols available in
``libmpi``.  In order to make the symbols in ``libmpi`` available to
the components loaded by Open MPI at runtime, your application must load
``libmpi`` with the ``RTLD_GLOBAL`` option.

In C/C++, this option is specified as the second parameter to the
POSIX ``dlopen(3)`` function.

When using ``ctypes`` with Python, this can be done with the second
(optional) parameter to ``CDLL()``.  For example (shown below in Mac OS
X, where Open MPI's shared library name ends in ``.dylib``; other
operating systems use other suffixes, such as ``.so``):

.. code-block:: python

   from ctypes import *

   mpi = CDLL('libmpi.0.dylib', RTLD_GLOBAL)

   f = pythonapi.Py_GetArgcArgv
   argc = c_int()
   argv = POINTER(c_char_p)()
   f(byref(argc), byref(argv))
   mpi.MPI_Init(byref(argc), byref(argv))

   # Your MPI program here

   mpi.MPI_Finalize()

.. note:: The above is just an example showing dynamic loading.  If
          you want to use MPI in Python, you are much better off using
          `MPI4Py <https://mpi4py.github.io/>`_.

Other scripting languages should have similar options when dynamically
loading shared libraries.
