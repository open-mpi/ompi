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
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!

#include "ompi/mpi/fortran/configure-fortran-output.h"

module mpi_f08

  use mpi_f08_types
  use mpi_f08_interfaces  ! this module contains the  mpi_f08 interface declarations
  use pmpi_f08_interfaces ! this module contains the pmpi_f08 interface declarations

!
! Declaration of the interfaces to the ompi impl files
! e.g., send_f.c
!
#include "mpi-f-interfaces-bind.h"
#include "pmpi-f-interfaces-bind.h"

! The MPI attribute callback functions

  include "attr-fn-f08-callback-interfaces.h"

! The MPI_CONVERSION_FN_NULL function

  include "conversion-fn-null-f08-interface.h"

! The sizeof interfaces

  include "sizeof_f08.h"

end module mpi_f08
