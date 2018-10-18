! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
! Copyright (c) 2016      Research Organization for Information Science
!                         and Technology (RIST). All rights reserved.
! Copyright (c) 2018      Los Alamos National Security, LLC. All rights
!                         reserved.
! $COPYRIGHT$
!
! Additional copyrights may follow
!
! $HEADER$
!

interface PMPI_Attr_delete

subroutine PMPI_Attr_delete(comm, keyval, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Attr_delete

end interface


interface PMPI_Attr_get

subroutine PMPI_Attr_get(comm, keyval, attribute_val, flag, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(out) :: attribute_val
  logical, intent(out) :: flag
  integer, intent(out) :: ierror
end subroutine PMPI_Attr_get

end interface


interface PMPI_Attr_put

subroutine PMPI_Attr_put(comm, keyval, attribute_val, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: keyval
  integer, intent(in) :: attribute_val
  integer, intent(out) :: ierror
end subroutine PMPI_Attr_put

end interface

interface PMPI_Errhandler_create

subroutine PMPI_Errhandler_create(function, errhandler, ierror)
  external :: function
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Errhandler_create

end interface

interface PMPI_Errhandler_get

subroutine PMPI_Errhandler_get(comm, errhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(out) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Errhandler_get

end interface


interface PMPI_Errhandler_set

subroutine PMPI_Errhandler_set(comm, errhandler, ierror)
  integer, intent(in) :: comm
  integer, intent(in) :: errhandler
  integer, intent(out) :: ierror
end subroutine PMPI_Errhandler_set

end interface

interface PMPI_Keyval_create

subroutine PMPI_Keyval_create(copy_fn, delete_fn, keyval, extra_state, ierror)
  external :: copy_fn
  external :: delete_fn
  integer, intent(out) :: keyval
  integer, intent(in) :: extra_state
  integer, intent(out) :: ierror
end subroutine PMPI_Keyval_create

end interface


interface PMPI_Keyval_free

subroutine PMPI_Keyval_free(keyval, ierror)
  integer, intent(inout) :: keyval
  integer, intent(out) :: ierror
end subroutine PMPI_Keyval_free

end interface

interface PMPI_Type_extent

subroutine PMPI_Type_extent(datatype, extent, ierror)
  integer, intent(in) :: datatype
  integer, intent(out) :: extent
  integer, intent(out) :: ierror
end subroutine PMPI_Type_extent

end interface

interface PMPI_Type_hindexed

subroutine PMPI_Type_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_hindexed

end interface


interface PMPI_Type_hvector

subroutine PMPI_Type_hvector(count, blocklength, stride, oldtype, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, intent(in) :: blocklength
  integer, intent(in) :: stride
  integer, intent(in) :: oldtype
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_hvector

end interface

interface PMPI_Type_lb

subroutine PMPI_Type_lb(datatype, lb, ierror)
  integer, intent(in) :: datatype
  integer, intent(out) :: lb
  integer, intent(out) :: ierror
end subroutine PMPI_Type_lb

end interface

interface PMPI_Type_struct

subroutine PMPI_Type_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype&
        , ierror)
  integer, intent(in) :: count
  integer, dimension(*), intent(in) :: array_of_blocklengths
  integer, dimension(*), intent(in) :: array_of_displacements
  integer, dimension(*), intent(in) :: array_of_types
  integer, intent(out) :: newtype
  integer, intent(out) :: ierror
end subroutine PMPI_Type_struct

end interface


interface PMPI_Type_ub

subroutine PMPI_Type_ub(datatype, ub, ierror)
  integer, intent(in) :: datatype
  integer, intent(out) :: ub
  integer, intent(out) :: ierror
end subroutine PMPI_Type_ub

end interface
