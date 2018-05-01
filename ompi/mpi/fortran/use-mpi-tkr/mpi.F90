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
! Copyright (c) 2016-2017 Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
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
  include "mpif-io-constants.h"
  include "mpif-io-handles.h"
  include "mpif-sentinels.h"

! The MPI attribute callback functions

  include "ompi/mpi/fortran/base/attr-fn-int-callback-interfaces.h"

! The MPI_CONVERSION_FN_NULL function

  include "ompi/mpi/fortran/base/conversion-fn-null-int-interface.h"

! Functions that have overloaded interfaces with TYPE(C_PTR) (which
! this compiler may or may not support).  We use an "if" preprocessor
! macro in this file, so we need to use the preprocessor include
! directive, not the Fortran include.
#include "mpi-f90-cptr-interfaces.h"
#include "pmpi-f90-cptr-interfaces.h"

! This file is generated, and is *huge*.  Its size is directly related
! to the --with-f90-max-array-dim configure parameter.

  include "mpi-f90-interfaces.h"
  include "pmpi-f90-interfaces.h"
#if OMPI_PROVIDE_MPI_FILE_INTEFACE
  include "mpi-f90-file-interfaces.h"
  include "pmpi-f90-file-interfaces.h"
#endif

#if OMPI_FORTRAN_BUILD_SIZEOF
  include "mpi-tkr-sizeof.h"
#endif

#if !defined(OMPI_ENABLE_MPI1_COMPAT)

#error "Remove MPI-1 compat code"

#elif OMPI_ENABLE_MPI1_COMPAT
  include "mpi-f90-removed-interfaces.h"
  include "pmpi-f90-removed-interfaces.h"
#endif

end module mpi
