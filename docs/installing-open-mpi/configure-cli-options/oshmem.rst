.. This file is included by building-open-mpi.rst

OpenSHMEM functionality
^^^^^^^^^^^^^^^^^^^^^^^

The following are command line options to set the default for various
OpenSHMEM API behaviors that can be used with ``configure``:

* ``--disable-oshmem``:
  Disable building the OpenSHMEM implementation (by default, it is
  enabled).

* ``--disable-oshmem-fortran``:
  Disable building only the Fortran OpenSHMEM bindings. Please see
  the "Compiler Notes" section herein which contains further
  details on known issues with various Fortran compilers.
