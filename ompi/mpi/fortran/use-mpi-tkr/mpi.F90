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
! Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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

! The MPI attribute callback functions need to be explictly called out
! so that they don't end up in the MPI namespace.  See a longer
! explanation in attr_fn-f90-interfaces.h.

  include "attr_fn-f90-interfaces.h"

! Similarly, we need the MPI_CONVERSION_FN_NULL function

  include "conversion_fn_null-f90-interface.h"

! This file is generated, and is *huge*.  Its size is directly related
! to the --with-f90-max-array-dim configure parameter.

  include "mpi-f90-interfaces.h"

end module mpi
