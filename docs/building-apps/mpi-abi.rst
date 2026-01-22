.. _label-building-with-mpi-abi:

Building MPI applications using the MPI ABI
===========================================

What is the MPI ABI
-------------------

Starting with MPI 5.0, the MPI standard specifies an ABI for the c and
Fortran MPI interfaces.  In this release, Open MPI supports the c
part of the MPI ABI.

By using the MPI ABI, an MPI application can be built against one 
implementation of MPI that supports the MPI ABI, and later run using
the MPI ABI compliant MPI library generated using a different MPI
implementation.  This assumes that the application is dynamically linked.
Although not mentioned explicitly in the standard, it is assumed that
a job launcher suitable for the MPI library used at runtime will need
to be used.

The MPI 5.0 standard specifies the file name of the MPI ABI compliant
library - libmpi_abi.  The major version of the library is 1 and minor
version is 0.

How to build an application using the MPI ABI
---------------------------------------------

To build an application against the MPI ABI compliant Open MPI library,
the ``mpicc_abi`` compiler wrapper must be used for compiling and 
linking the application or shared library.

This release does not support the Fortran ABI so there is no ``mpifort_abi``
compiler wrapper.  This mixed c/Fortran MPI apps cannot make use of the
MPI ABI library with this release.


