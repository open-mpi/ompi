Source code: Symbol visibility
==============================

The ``*_DECLSPEC`` macros provide a method to annotate symbols to indicate
their intended visibility when compiling dynamically shared object files
(e.g., ``libmpi.so``).

The macros are defined on a per project basis:

* Open MPI: ``OMPI_DECLSPEC``
* Open PAL: ``OPAL_DECLSPEC``
* OpenSHMEM: ``OSHMEM_DECLSPEC``

The macros expand to the appropriate compiler and platform flags for marking
whether a symbol should be explicitly made public in the target project's
library namespace.  This is related to the Windows `__declspec <https://docs.microsoft.com/en-us/cpp/cpp/declspec?view=msvc-170>`_.

The ``*_DECLSPEC`` attributes are used to declare that a symbol is to be
visible outside of that library/DSO's scope.  For example, ``OMPI_DECLSPEC``
is used to control what symbols are visible in the ``libmpi.so`` scope.

.. note:: This is entirely related to dynamic library compilation and does not
   apply to static compilation.
