# Open MPI extension: Example

## Overview

This example MPI extension shows how to make an MPI extension for Open
MPI.

An MPI extension provides new top-level APIs in Open MPI that are
available to user-level applications (vs. adding new code/APIs that is
wholly internal to Open MPI).  MPI extensions are generally used to
prototype new MPI APIs, or provide Open MPI-specific APIs to
applications.  This example MPI extension provides a new top-level MPI
API named `OMPI_Progress` that is callable in both C and Fortran.

MPI extensions are similar to Open MPI components, but due to
complex ordering requirements for the Fortran-based MPI bindings,
their build order is a little different.

Note that MPI has 4 different sets of bindings (C, Fortran `mpif.h`,
the Fortran `mpi` module, and the Fortran `mpi_f08` module), and Open
MPI extensions allow adding API calls to all 4 of them.  Prototypes
for the user-accessible functions/subroutines/constants are included
in the following publicly-available mechanisms:

* C: `mpi-ext.h`
* Fortran mpif.h: `mpif-ext.h`
* Fortran "use mpi": `use mpi_ext`
* Fortran "use mpi_f08": `use mpi_f08_ext`

This example extension defines a new top-level API named
`OMPI_Progress()` in all four binding types, and provides test programs
to call this API in each of the four binding types.  Code (and
comments) is worth 1,000 words -- see the code in this example
extension to understand how it works and how the build system builds
and inserts each piece into the publicly-available mechansisms (e.g.,
`mpi-ext.h` and the `mpi_f08_ext` module).

## Comparison to General Open MPI MCA Components

Here's the ways that MPI extensions are similar to Open MPI
components:

1. Extensions have a top-level `configure.m4` with a well-known m4 macro
   that is run during Open MPI's configure that determines whether the
   component wants to build or not.

   Note, however, that unlike components, extensions *must* have a
   `configure.m4`.  No other method of configuration is supported.

1. Extensions must adhere to normal Automake-based targets.  We
   strongly suggest that you use `Makefile.am`'s and have the
   extension's `configure.m4` `AC_CONFIG_FILE` each `Makefile.am` in
   the extension.  Using other build systems may work, but are
   untested and unsupported.

1. Extensions create specifically-named libtool convenience archives
   (i.e., `*.la` files) that the build system slurps into higher-level
   libraries.

Unlike components, however, extensions:

1. Have a bit more rigid directory and file naming scheme.
1. Have up to four different, specifically-named subdirectories (one
   for each MPI binding type).
1. Also install some specifically-named header files (for C and the
   Fortran `mpif.h` bindings).

Similar to components, an MPI extension's name is determined by its
directory name: `ompi/mpiext/EXTENSION_NAME`

## Extension requirements

### Required: C API

Under this top-level directory, the extension *must* have a directory
named `c` (for the C bindings) that:

1. contains a file named `mpiext_EXTENSION_NAME_c.h`
1. installs `mpiext_EXTENSION_NAME_c.h` to
   `$includedir/openmpi/mpiext/EXTENSION_NAME/c`
1. builds a Libtool convenience library named
   `libmpiext_EXTENSION_NAME_c.la`

### Optional: `mpif.h` bindings

Optionally, the extension may have a director named `mpif-h` (for the
Fortran `mpif.h` bindings) that:

1. contains a file named `mpiext_EXTENSION_NAME_mpifh.h`
1. installs `mpiext_EXTENSION_NAME_mpih.h` to
   `$includedir/openmpi/mpiext/EXTENSION_NAME/mpif-h`
1. builds a Libtool convenience library named
   `libmpiext_EXTENSION_NAME_mpifh.la`

### Optional: `mpi` module bindings

Optionally, the extension may have a directory named `use-mpi` (for the
Fortran `mpi` module) that:

1. contains a file named `mpiext_EXTENSION_NAME_usempi.h`

***NOTE:*** The MPI extension system does NOT support building an
additional library in the `use-mpi` extension directory.  It is
assumed that the `use-mpi` bindings will use the same back-end symbols
as the `mpif.h` bindings, and that the only output product of the
`use-mpi` directory is a file to be included in the `mpi-ext` module
(i.e., strong Fortran prototypes for the functions/global variables in
this extension).

### Optional: `mpi_f08` module bindings

Optionally, the extension may have a directory named `use-mpi-f08` (for
the Fortran `mpi_f08` module) that:

1. contains a file named `mpiext_EXTENSION_NAME_usempif08.h`
1. builds a Libtool convenience library named
   `libmpiext_EXTENSION_NAME_usempif08.la`

See the comments in all the header and source files in this tree to
see what each file is for and what should be in each.

## Notes

Note that the build order of MPI extensions is a bit strange.  The
directories in a MPI extensions are NOT traversed top-down in
sequential order.  Instead, due to ordering requirements when building
the Fortran module-based interfaces, each subdirectory in extensions
are traversed individually at different times in the overall Open MPI
build.

As such, `ompi/mpiext/EXTENSION_NAME/Makefile.am` is not traversed
during a normal top-level `make all` target.  This `Makefile.am`
exists for two reasons, however:

1. For the conveneince of the developer, so that you can issue normal
   `make` commands at the top of your extension tree (e.g., `make all`
   will still build all bindings in an extension).
1. During a top-level `make dist`, extension directories *are*
   traversed top-down in sequence order.  Having a top-level
   `Makefile.am` in an extension allows `EXTRA_DIST`ing of files, such
   as this `README.md` file.

This are reasons for this strange ordering, but suffice it to say that
`make dist` doesn't have the same ordering requiements as `make all`,
and is therefore easier to have a "normal" Automake-usual top-down
sequential directory traversal.

Enjoy!
