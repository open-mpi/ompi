! -*- fortran -*-
!
! Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

! This whole file will be included in the mpi_f08_ext module interface
! section.

type(MPI_Datatype), bind(C, name="ompi_f08_mpi_short_float") OMPI_PROTECTED :: MPIX_SHORT_FLOAT
type(MPI_Datatype), bind(C, name="ompi_f08_mpi_c_short_float_complex") OMPI_PROTECTED :: MPIX_C_SHORT_FLOAT_COMPLEX
type(MPI_Datatype), bind(C, name="ompi_f08_mpi_cxx_short_float_complex") OMPI_PROTECTED :: MPIX_CXX_SHORT_FLOAT_COMPLEX
