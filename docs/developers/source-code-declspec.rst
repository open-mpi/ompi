Source code: Symbol visibility
==============================

The `*_DECLSPEC` macros provide a method to annotate symbols to indicate
their intended visibility when compiling dynamically shared object files
(e.g., `libmpi.so`).

The macros are defined on a per project basis:

    * Open MPI: `OMPI_DECLSPEC`, `OMPI_MODULE_DECLSPEC`
    * Open PAL: `OPAL_DECLSPEC`, `OPAL_MODULE_DECLSPEC`
    * OpenSHMEM: `OSHMEM_DECLSPEC`, `OSHMEM_MODULE_DECLSPEC`

The macros expand to the appropriate compiler and platform flags for marking
whether a symbol should be explicitly made public in the target projects
library namespace.  This is related to the Windows [declspec](https://docs.microsoft.com/en-us/cpp/cpp/declspec?view=msvc-170).

The `*_DECLSPEC` attribute is used to declare that the developer wants a
symbol to be usable outside of that library/DSO.

For example, `OMPI_DECLSPEC` is used to control what symbols are visible in
the `libmpi.so` scope.

*NOTE*: This is entirely related to dynamic library compilation and does not
        apply to static compilation.

There are two variants of the macro:

    * `*_DECLSPEC`: controls visibility on individual functions/data structures
    * `*_MODULE_DECLSPEC`: controls visibility of MCA module (component) structure

