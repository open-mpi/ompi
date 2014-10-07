! -*- f90 -*-
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
! Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!

#include "ompi/mpi/fortran/configure-fortran-output.h"

module mpi

  include "mpif-config.h"
  include "mpif-constants.h"
  include "mpif-handles.h"
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  include "mpif-io-constants.h"
  include "mpif-io-handles.h"
#endif
  include "mpif-sentinels.h"

! The MPI attribute callback functions

  include "ompi/mpi/fortran/base/attr-fn-int-callback-interfaces.h"

! Similarly, we need the MPI_CONVERSION_FN_NULL function

  include "ompi/mpi/fortran/base/conversion-fn-null-int-interface.h"

! The ignore-TKR version of the MPI interfaces

  include "ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-interfaces.h"
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
  include "ompi/mpi/fortran/use-mpi-ignore-tkr/mpi-ignore-tkr-file-interfaces.h"
#endif

  include 'mpi-ignore-tkr-sizeof.h'

end module mpi
