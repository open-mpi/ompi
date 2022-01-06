! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2010 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

!
!     All of these types were chosen with care to match the types of
!     their corresponding C variables.  Do not arbitrarily change
!     their types without also updating:
!
!     - the "mpi" module bindings
!     - the "mpi_f08" module bindings
!     - ompi/mpi/fortran/base/gen-mpi-mangling.pl
!

!     MPI_BOTTOM is only used where choice buffers can be used (meaning
!     that we already have overloaded F90 bindings for all available
!     types), so any type is fine.
      integer MPI_BOTTOM
      common/mpi_fortran_bottom/MPI_BOTTOM
      bind(C, name="mpi_fortran_bottom")/mpi_fortran_bottom/

!     MPI_IN_PLACE has the same rationale as MPI_BOTTOM.
      integer MPI_IN_PLACE
      common/mpi_fortran_in_place/MPI_IN_PLACE
      bind(C, name="mpi_fortran_in_place")/mpi_fortran_in_place/

!     Making MPI_ARGV_NULL be the same type as the parameter that is
!     exepected in the F90 binding for MPI_COMM_SPAWN means that we
!     don't need another interface for MPI_COMM_SPAWN.
      character MPI_ARGV_NULL(1)
      common/mpi_fortran_argv_null/MPI_ARGV_NULL
      bind(C, name="mpi_fortran_argv_null")/mpi_fortran_argv_null/

!     Ditto for MPI_ARGVS_NULL / MPI_COMM_SPAWN_MULTIPLE.
      character MPI_ARGVS_NULL(1, 1)
      common/mpi_fortran_argvs_null/MPI_ARGVS_NULL
      bind(C, name="mpi_fortran_argvs_null")/mpi_fortran_argvs_null/

!     MPI_ERRCODES_IGNORE has similar rationale to MPI_ARGV_NULL.  The
!     F77 functions are all smart enough to check that the errcodes
!     parameter is not ERRCODES_IGNORE before assigning values into it
!     (hence, the fact that this is an array of only 1 element does not
!     matter -- we'll never overrun it because we never assign values
!     into it).
      integer MPI_ERRCODES_IGNORE(1)
      common/mpi_fortran_errc_ign/MPI_ERRCODES_IGNORE
      bind(C, name="mpi_fortran_errcodes_ignore")/mpi_fortran_errc_ign/

!     Ditto for MPI_UNWEIGHTED
      integer MPI_UNWEIGHTED(1)
      common/mpi_fortran_unwghtd/MPI_UNWEIGHTED
      bind(C, name="mpi_fortran_unweighted")/mpi_fortran_unwghtd/

!     Ditto for MPI_WEIGHTS_EMPTY
      integer MPI_WEIGHTS_EMPTY(1)
      common/mpi_fortran_wghts_empty/MPI_WEIGHTS_EMPTY
      bind(C, name="mpi_fortran_weights_empty")/mpi_fortran_wghts_empty/
