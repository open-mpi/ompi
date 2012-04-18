!
! Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!

module mpi

  include "mpif-common.h"

! The MPI attribute callback functions need to be explictly called out
! so that they don't end up in the MPI namespace.  See a longer
! explanation in attr_fn-f90-interfaces.h.

  include "ompi/mpi/fortran/use-mpi-tkr/attr_fn-f90-interfaces.h"

! Similarly, we need the MPI_CONVERSION_FN_NULL function

  include "ompi/mpi/fortran/use-mpi-tkr/conversion_fn_null-f90-interface.h"

! The ignore-TKR version of the MPI interfaces

  include "ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-interfaces.h"
#if OMPI_PROVIDE_FILE_INTERFACE
  include "ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-file-interfaces.h"
#endif

end module mpi
