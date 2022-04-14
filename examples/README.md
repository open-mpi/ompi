The files in this directory are sample MPI applications provided both
as a trivial primer to MPI as well as simple tests to ensure that your
Open MPI installation is working properly.

If you are looking for a comprehensive MPI tutorial, these samples are
not enough.  [Excellent MPI tutorials are available
here](http://www.hpc-training.org/).

Get a free account and login; you can then browse to the list of
available courses.  Look for the ones with "MPI" in the title.

There are two MPI examples in this directory, each using one of six
different MPI interfaces:

## Hello world

The MPI version of the canonical "hello world" program:

* C: `hello_c.c`
* C++: `hello_cxx.cc`
* Fortran mpif.h: `hello_mpifh.f`
* Fortran use mpi: `hello_usempi.f90`
* Fortran use mpi_f08: `hello_usempif08.f90`
* Java: `Hello.java`
* C shmem.h: `hello_oshmem_c.c`
* Fortran shmem.fh: `hello_oshmemfh.f90`

## Ring

Send a trivial message around in a ring:

* C: `ring_c.c`
* C++: `ring_cxx.cc`
* Fortran mpif.h: `ring_mpifh.f`
* Fortran use mpi: `ring_usempi.f90`
* Fortran use mpi_f08: `ring_usempif08.f90`
* Java: `Ring.java`
* C shmem.h: `ring_oshmem_c.c`
* Fortran shmem.fh: `ring_oshmemfh.f90`

## Connectivity Test

Additionally, there's one further example application, but this one
only uses the MPI C bindings to test the connectivity between all
processes:

* C: `connectivity_c.c`

## Makefile

The `Makefile` in this directory will build as many of the examples as
you have language support (e.g., if you do not have the Fortran `use
mpi` bindings compiled as part of Open MPI, the those examples will be
skipped).

The `Makefile` assumes that the wrapper compilers `mpicc`, `mpic++`, and
`mpifort` are in your path.

Although the `Makefile` is tailored for Open MPI (e.g., it checks the
`ompi_info` command to see if you have support for `mpif.h`, the `mpi`
module, and the `use mpi_f08` module), all of the example programs are
pure MPI, and therefore not specific to Open MPI.  Hence, you can use
a different MPI implementation to compile and run these programs if
you wish.

Make today an Open MPI day!
