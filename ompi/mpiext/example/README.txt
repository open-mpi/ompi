Copyright (C) 2012 Cisco Systems, Inc.  All rights reserved.

$COPYRIGHT$

This example MPI extension shows how to make an MPI extension for Open
MPI.  

An MPI extension provides new top-level APIs in Open MPI that are
available to user-level applications (vs. adding new code/APIs that is
wholly internal to Open MPI).  MPI extensions are generally used to
prototype new MPI APIs, or provide Open MPI-specific APIs to
applications.  This example MPI extension provides a new top-level MPI
API named "OMPI_Progress" that is callable in both C and Fortran.

MPI extensions are similar to Open MPI components, but due to
complex ordering requirements for the Fortran-based MPI bindings,
their build order is a little different.

Note that MPI has 4 different sets of bindings (C, Fortran mpif.h,
Fortran "use mpi", and Fortran "use mpi_f08"), and Open MPI extensions
allow adding API calls to all 4 of them.  Prototypes for the
user-accessible functions/subroutines/constants are included in the
following publicly-available mechanisms:

- C: mpi-ext.h
- Fortran mpif.h: mpif-ext.h
- Fortran "use mpi": use mpi_ext
- Fortran "use mpi_f08": use mpi_f08_ext

This example extension defines a new top-level API named
"OMPI_Progress" in all four binding types, and provides test programs
to call this API in each of the four binding types.  Code (and
comments) is worth 1,000 words -- see the code in this example
extension to understand how it works and how the build system builds
and inserts each piece into the publicly-available mechansisms (e.g.,
mpi-ext.h and the mpi_f08_ext module).

--------------------------------------------------------------------------------

Here's the ways that MPI extensions are similar to Open MPI
components:

- Extensions have a top-level configure.m4 with a well-known m4 macro
  that is run during Open MPI's configure that determines whether the
  component wants to build or not.

  Note, however, that unlike components, extensions *must* have a
  configure.m4.  No other method of configuration is supported.

- Extensions must adhere to normal Automake-based targets.  We
  strongly suggest that you use Makefile.am's and have the extension's
  configure.m4 AC_CONFIG_FILE each Makefile.am in the extension.
  Using other build systems may work, but are untested and
  unsupported.

- Extensions create specifically-named libtool convenience archives
  (i.e., *.la files) that the build system slurps into higher-level
  libraries.

Unlike components, however, extensions:

- Have a bit more rigid directory and file naming scheme.

- Have up to four different, specifically-named subdirectories (one
  for each MPI binding type).

- Also install some specifically-named header files (for C and the
  Fortran mpif.h bindings).

Similar to components, an MPI extension's name is determined by its
directory name: ompi/mpiext/<extension name>

Under this top-level directory, the extension *must* have a directory
named "c" (for the C bindings) that:

- contains a file named mpiext_<ext_name>_c.h
- installs mpiext_<ext_name>_c.h to 
  $includedir/openmpi/mpiext/<ext_name>/c
- builds a Libtool convenience library named libmpiext_<ext_name>_c.la

Optionally, the extension may have a director named "mpif-h" (for the
Fortran mpif.h bindings) that:

- contains a file named mpiext_<ext_name>_mpifh.h
- installs mpiext_<ext_name>_mpih.h to 
  $includedir/openmpi/mpiext/<ext_name>/mpif-h
- builds a Libtool convenience library named libmpiext_<ext_name>_mpifh.la

Optionally, the extension may have a director named "use-mpi" (for the
Fortran "use mpi" bindings) that:

- contains a file named mpiext_<ext_name>_usempi.h

NOTE: The MPI extension system does NOT support building an additional
library in the use-mpi extension directory.  It is assumed that the
use-mpi bindings will use the same back-end symbols as the mpif.h
bindings, and that the only output product of the use-mpi directory is
a file to be included in the mpi-ext module (i.e., strong Fortran
prototypes for the functions/global variables in this extension).

Optionally, the extension may have a director named "use-mpi-f08" (for
the Fortran mpi_f08 bindings) that:

- contains a file named mpiext_<ext_name>_usempif08.h
- builds a Libtool convenience library named
  libmpiext_<ext_name>_usempif08.la

See the comments in all the header and source files in this tree to
see what each file is for and what should be in each.

--------------------------------------------------------------------------------

Note that the build order of MPI extensions is a bit strange.  The
directories in a MPI extensions are NOT traversed top-down in
sequential order.  Instead, due to ordering requirements when building
the Fortran module-based interfaces, each subdirectory in extensions
are traversed individually at different times in the overall Open MPI
build.

As such, ompi/mpiext/<ext_name>/Makefile.am is not traversed during a
normal top-level "make all" target.  This Makefile.am exists for two
reasons, however:

1. For the conveneince of the developer, so that you can issue normal
"make" commands at the top of your extension tree (e.g., "make all"
will still build all bindings in an extension).

2. During a top-level "make dist", extension directories *are*
traversed top-down in sequence order.  Having a top-level Makefile.am
in an extension allows EXTRA_DISTing of files, such as this README
file.

This are reasons for this strange ordering, but suffice it to say that
"make dist" doesn't have the same ordering requiements as "make all",
and is therefore easier to have a "normal" Automake-usual top-down
sequential directory traversal.  

Enjoy!
