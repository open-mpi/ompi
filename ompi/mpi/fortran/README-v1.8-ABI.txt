December 2014

Some unfortunate things happened in the v1.8 series with regards to
the Fortran MPI interface ABI.

-------------------------------------------------

Let's summarize in terms of two main items:

1. Fortran compiler support:

1a. gfortran 4.4.7 (i.e., "old" gfortran)
1b. gfortran 4.9 (i.e., "new" gfortran)
1c. ifort 15

The first (gfortran 4.4.7) does not contain enough modern Fortran
support for the mpi_f08 module.  It also does not support ignore TKR
pragmas to support the ignore-tkr mpi module implementation.

gfortran 4.9 is the first version that supported enough modern Fortran
to support the mpi_f08 module as well as the ignore TKR pragmas, and
therefore supports the ignore-tkr mpi module implementation.

ifort has long since supported enough Fortran for mpi_f08 and ignore
TKR pragmas.

2. MPI_SIZEOF implementation.

In v1.8.3, it was recongized that MPI_SIZEOF was supposed to be
included in mpif.h.  It was also recongized that the MPI_SIZEOF
interfaces that were available (previously in the mpi module) were
inadequate and configuring.  So they were re-implemented in a much
more straightforward way.  The MPI_SIZEOF interfaces in the mpi_f08
module were also re-implemented.

-------------------------------------------------

In v1.8, here's what was available:

- old gfortran:
  --> TKR implementation of mpi module (libmpi_usempi.so),
      including the "old" implementation of MPI_SIZEOF.
  --> no mpi_f08 module
- new gfortran: TKR implementation of mpi module (libmpi_usempi.so),
  --> TKR implementation of mpi module (libmpi_usempi.so),
      including the "old" implementation of MPI_SIZEOF.
  --> no mpi_f08 module
- ifort:
  --> ignore TKR implementation of mpi module (libmpi_usempi_ignore_tkr.so)
  --> installed the mpi_f08 module, including the CONTAINS
      implementation of SIZEOF

Note that configure didn't distinguish between old and new gfortran --
even though new gfortran *could* support the new stuff, configure
didn't figure that our, so therefore old and new gfortran were the
same according to OMPI.

v1.8.1 was the same.

In v1.8.2, however, configure grew some new tests that allowed new
gfortran to support the ignore TKR mpi module and the mpi_f08 module.
Hence, we got this:

- old gfortran:
  --> TKR implementation of mpi module (libmpi_usempi.so),
      including the "old" implementation of MPI_SIZEOF.
  --> no mpi_f08 module
- new gfortran:
  --> ignore TKR implementation of mpi module (libmpi_usempi_ignore_tkr.so)
  --> installed the mpi_f08 module, including the CONTAINS
      implementation of SIZEOF
- ifort: ignore TKR implementation of mpi module (libmpi_usempi_ignore_tkr.so)
  --> ignore TKR implementation of mpi module (libmpi_usempi_ignore_tkr.so)
  --> installed the mpi_f08 module, including the CONTAINS
      implementation of SIZEOF

Notice that if you have new gfortran, you suddenly get something
different (to include a different library name).

:-(

v1.8.3 is the same as v1.8.2.

-------------------------------------------------

This means that there are multiple points of ABI breakage here:

1. new gfortran users got a new library name at 1.8.2
2. all apps that call MPI_SIZEOF from the mpi module had their symbols
   changed in 1.8.4
3. all apps that call MPI_SIZEOF from the mpi_f08 module had their
   symbols changed in 1.8.4

Multiple things were done to fix this in 1.8.4 (and beyond):

1. If gfortran is being used to build the Fortran interfaces *and*
   we're building the ignore-tkr mpi module, then *also* build/install
   a dummy libmpi.so.  This allows any old app (i.e., that was
   compiled/linked against <=v1.8.2) to find the shared library that
   it expects.

2. A copy of the old MPI_SIZEOF mpi module implementation subroutines
   are compiled into libmpi_mpif.la.  These subroutines are not
   referred to from anywhere, nor are they included in the MPI_SIZEOF
   interface.  They are just compiled in the library so that if any
   old app (i.e., that was compiled/linked against <=v1.8.2) uses
   MPI_SIZEOF, it'll be able to find/link against the symbols that it
   expects.

3. The <=v1.8.2 mpi_f08 module uses a CONTAINS implementation of
   MPI_SIZEOF (which is the moral equivalent of C inlining).  However,
   CONTAINS is only a compiler *hint*; it doesn't *require* inlining.
   Hence, there may well be symbols in a shared library that an app
   requires.  So similar to #2, the subroutines used to implement
   MPI_SIZEOF in the mpi_f08 module are simply compiled into the
   mpi_f08 module library.  They're not referred to anywhere, nor are
   they part of the MPI_SIZEOF interface (although they do have to be
   in a module of the same name as they were in <=v1.8.2, and also be
   implemented with CONTAINS so that the compiler generates the same
   symbols that it did in <=v1.8.2).  The resulting .o file is linked
   into libmpi_mpifh.la, so if an old app needs those symbols, they
   will be found.

NOTE: The wrapper compilers were *not* updated to add in the dummy
library from #1.  The dummy library is *only* necessary for apps that
were compiled/linked against OMPI <= v1.8.3 and are trying to run with
OMPI >= v1.8.4.  Hence, apps compiled/linked against OMPI >= v1.8.4 do
not need to be linked against the dummy library.

-------------------------------------------------

As a related note, during the process of cleaning up all this ABI
breakage, it was discovered that the library for the ignore-tkr mpi
module implementation was not assigning its shared library version
properly (i.e., it was always defaulting to 0:0:0).

Hence, for the v1.8-v1.8.2, the shared library version for the
ignore-tkr mpi module is 0:0:0.  For v1.8.3, it was set to 1:1:1.

-------------------------------------------------

Long live Fortran!
