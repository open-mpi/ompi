# MPI-5 ABI support

Last updated: December 2025

This README describes the current approach to supporting the MPI-5 
Application Binary Interface (ABI).  The MPI ABI is described in 
Chapter 20 of the MPI-5 standard.  The standardized values for
constants, etc. are presented in Appendix A of that document.

## What does having a defined ABI mean to applications?

By supporting a standardized ABI, an MPI application can be built
against one implementation of MPI (e.g.) MPICH, but at runtime
can use the ABI compatible Open MPI implementation of the library.
This only applies to dynamically linked applications.

In principle a single mpi.h include file can be used, for example
the one provided at https://github.com/mpi-forum/mpi-abi-stubs.

The MPI ABI specifies the file name of the ABI MPI
library (libmpi_abi). It does not explicitly specify the shared library
name(SONAME),  but it presumably is libmpi_abi.0. Note we need for this to
by the same name as that used by the MPICH implementation of the MPI ABI.
The major and minor versions of the library are specified in this chapter as well.
See the top level VERSION file for adjusting the SONAME.

The name of the compiler wrapper - mpicc_abi - is also specified.

## Refactor of Open MPI to support the MPI ABI

Relatively few changes were made to the internal components of Open MPI
to support the ABI, but the way in which it is structured in terms of
internal libraries has been changed.

There are now two top level libraries, one to support the OMPI ABI, 
and the other the MPI ABI.  The internal components of Open MPI are now
placed in separate library, against which both of the ABI specific 
libraries are linked.  The structure is depicted below:

         MPI ABI                 OMPI ABI
  +----------------------+----------------------+
  |     libmpi_abi.la    |       libmpi.la      |
  +----------------------+----------------------+
  |              libopen_mpi.la                 |
  +----------------------+----------------------+
 
Note the ability to change the name of the OMPI ABI library (libmpi.la)
is still supported, so in the code base Makefiles this library actually
appears as lib@OMPI_LIBMPI_NAME@.la).

The Fortran OMPI libraries are linked against libmpi.so.0.

## Installation considerations

The MPI and OMPI libraries are installed in the same <install-path>/lib
folder.  They have different file names so this presents no problems.
The OMPI ABI include files are installed in <install-path>/include as usual.
The MPI ABI include file is installed in <install-path>/include/standard_abi.
The mpicc_abi compiler wrapper points the c compiler to the MPI ABI mpi.h and
links the executable/shared library against libmpi_abi.

## Generating the ABI compliant mpi.h

Rather than use the mpi.h at https://github.com/mpi-forum/mpi-abi-stubs, 
it was decided to develop infrastructure for building it from the latex
content of the MPI standard.  It turns out that this was the better route
as bugs were found both in the standard itself and the mpi-abi-stubs repo
mpi.h while developing this infrastructure.

Generation of the mpi.h (and a shadow abi.h) include files involves four
components:

1. pympistandard at https://github.com/mpi-forum/pympistandard.  This
   is now included in the 3rd-party folder.

2. two json files: mpi-standard-apis.json and mpi-standard-abi.json.
   The former is generated as part of the build process for the MPI standard.
   The later is generated via a tool (not yet incorporated in the upstream 
   MPI standard code base).  The tool converts the values embedded in the
   appendix A latex file into a json file specifying the values for MPI
   constants, e.g. value of MPI_COMM_WORLD. These json files reside in
   the top level docs folder.  Note the first json file is also used to
   generate makefile content.

3. A tool - c_header.py was written to parse the two json files 
   described above and generate the MPI ABI compliant mpi.h and a name mangled
   version - abi.h.  Eventually this tool will be incorporated into the
   binding infrastructure.  More on the binding infrastructure below.
   The tool uses methods from pympistandard.

4. The c_header.py tool uses a template file - abi.h.in located in
   the ompi/mpi/c folder to generate the MPI ABI mpi.h and the name
   mangled version - abi.h. The template is just used to structure how the
   various blocks of the resulting mpi.h are placed, not the actual content.
   The tool uses methods from pympistandard.

## Generating the MPI ABI compliant c bindings

The ABI compliant c bindings are generated using the binding infrastructure
originally developed to support big count.  The infrastructure was significantly
enhanced to support generation of the MPI ABI bindings.

This infrastructure uses templates (\*.c.in) files to generate up to four
variants:  OMPI ABI int count, OMPI ABI big count, MPI ABI int count, 
MPI ABI big count.  There are a small set of functions which have
only int count or big count.  The templates for these files have
suffixes of (\*.c.in_obc for big count only and \*.c.in_nbc for int
count only).

Note a number of procedures (e.g. all the _f2c/_c2f) are not present in 
the MPI ABI.  Likewise there are restrictions as to which deprecated
procedures are include in the MPI ABI.  These restrictions are managed
via the ompi/mpi/c/Makefile.am and ompi/mpi/c/Makefile_abi.include make
files.

Generation of the OMPI ABI sources files only involves the code changes
that were done to support big count.  

The MPI ABI source file generation is more complex.  The source files 
generated include both the OMPI ABI mpi.h and the name 
mangled MPI ABI abi.h include files.  Thus the generated source files 
have two definitions of MPI predefined constants.  As an example
in a MPI ABI source file there are two world communicator values defined:
MPI_COMM_WORLD, and MPI_COMM_WORLD_ABI_INTERNAL.  The former
is the OMPI ABI defintion of the world communicator(address of a
global variable) , the later being the MPI ABI value for this constant (an integer).

The bindings framework parses the input argument types to the function
and generates calls to appropriate converter methods to convert any MPI ABI
constants in the arguments to OMPI ABI ones.  A wrapped version
of the original function code is then called.  Any output arguments
that require conversion are converted from the OMPI ABI values back to MPI ABI ones.
This includes fields in MPI_Status objects which require conversion 
and error return values from the wrapped code.

The bindings framework also generates two helper files - abi_converter.h
and abi_converter.c in addition to the generated OMPI and MPI ABI
source files.

There are several implications of this approach.  

First very few changes have to made to the internal Open MPI source code.  

A second is that top level MPI ABI c entry points are expecting
argument values using MPI ABI constants.  Thus calling top 
level MPI functions from within libompi_mpi will not work 
properly.

A third is there are a few places in non-blocking/persistent 
methods where arrays of converted values need to be allocated,
and freed upon completion of the non-blocking collective or release
of the persistent request.

## Handling user defined functions

The standard specifies a number of user defined functions

1. attribute copy and delete functions
2. operator functions (for reduction operations)
3. errhandler functions
4. generalized request related functions
5. datarep functions
6. MPI_T event callback functions.

The MPI ABI support for these functions varies.  For attribute related
functions, a wrapper generation approach is taken.  The wrapper converts
OMPI ABI constants to MPI ABI ones as needed.  No changes are needed
to OMPI internal functions to support these.

Operator functions are handled by extending the internal support for
ops functions to allow for invocation of a translation routine to
convert the datatype argument from the OMPI ABI to MPI ABI values
as appropriate.  A similar approach is taken for error handlers.

Generalized requests don't need special support as the MPI Status
structure for both MPI and OMPI ABIs are similar enough as to not
require conversion.

OMPI doesn't really support datarep functions so currently there
is no need for any argument conversion operations for these functions.

The MPI_T event implementation is just a set of stubs so there's
no need for special support for MPI_T_event callback functions.
