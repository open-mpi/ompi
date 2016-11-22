! -*- f90 -*-
!
! Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!                         All rights reserved.
! $COPYRIGHT$

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Fortran equivalent of ISO_Fortran_binding.h
!   - this is used temporarily until compilers support the TR
!
module OMPI_Fortran_binding
   use mpi_f08_types
   use, intrinsic :: ISO_C_BINDING

   !
   ! The following types and procedures are here temporarily,
   ! for testing purposes only
   !

   integer, parameter :: INTPTR_T_KIND = C_INTPTR_T
   integer, parameter :: CFI_MAX_RANK = 7  ! until F2008 compilers

   type, bind(C) :: CFI_dim_t
      integer(INTPTR_T_KIND) :: lower_bound, extent, sm;
   end type CFI_dim_t

   type, bind(C) :: CFI_cdesc_t
      type(C_PTR)       :: base_addr    ! base address of object
      integer(C_SIZE_T) :: elem_len     ! length of one element, in bytes
      integer(C_INT)    :: rank         ! object rank, 0 .. CF_MAX_RANK
      integer(C_INT)    :: type         ! identifier for type of object
      integer(C_INT)    :: attribute    ! object attribute: 0..2, or -1
      integer(C_INT)    :: state        ! allocation/association state: 0 or 1
      type(CFI_dim_t)   :: dim(CFI_MAX_RANK)  ! dimension triples
   end type CFI_cdesc_t

   interface
      subroutine ompi_recv_f08_desc_f(desc,count,datatype,dest,tag,comm,status,ierror) &
         BIND(C, name="ompi_recv_f08_desc_f")
         use mpi_f08_types, only : MPI_Status
         import CFI_cdesc_t
         implicit none
         type(CFI_cdesc_t) :: desc
         INTEGER, INTENT(IN) :: count, dest, tag
         INTEGER, INTENT(IN) :: datatype
         INTEGER, INTENT(IN) :: comm
         TYPE(MPI_Status), INTENT(OUT) :: status
         INTEGER, INTENT(OUT) :: ierror
      end subroutine ompi_recv_f08_desc_f

      subroutine ompi_send_f08_desc_f(desc,count,datatype,dest,tag,comm,ierror) &
         BIND(C, name="ompi_send_f08_desc_f")
         import CFI_cdesc_t
         implicit none
         type(CFI_cdesc_t) :: desc
         INTEGER, INTENT(IN) :: count, dest, tag
         INTEGER, INTENT(IN) :: datatype
         INTEGER, INTENT(IN) :: comm
         INTEGER, INTENT(OUT) :: ierror
      end subroutine ompi_send_f08_desc_f

      function ompi_f08_addr(buf) &
        BIND(C, name="ompi_f08_addr")
        import :: C_PTR
        type(C_PTR), value :: buf
        type(C_PTR) :: ompi_f08_addr
      end function ompi_f08_addr

      subroutine ompi_f08_print_addr(buf) &
        BIND(C, name="ompi_f08_print_addr")
        import :: C_PTR
        type(C_PTR), value :: buf
      end subroutine ompi_f08_print_addr

      function ompi_f08_addr_diff(buf1, buf2) &
        BIND(C, name="ompi_f08_addr_diff")
        import :: C_PTR, C_SIZE_T
        type(C_PTR), value :: buf1, buf2
        integer(C_SIZE_T) :: ompi_f08_addr_diff
      end function ompi_f08_addr_diff
   end interface

contains

subroutine print_desc(desc)
   implicit none
   type(CFI_cdesc_t), intent(in) :: desc
   type(C_PTR) :: cptr
   integer :: i

   print *, "print_desc:"
   call ompi_f08_print_addr(desc%base_addr)
   print *, "   rank     =", desc%rank
   print *, "   elem_len =", desc%elem_len
   print *, "   type     =", desc%type
   print *, "   attribute=", desc%attribute
   print *, "   state    =", desc%attribute
   print *, "   dims     ="
   do i = 1, desc%rank
      print *, desc%dim(i)%lower_bound, desc%dim(i)%extent, desc%dim(i)%sm
   end do

end subroutine print_desc

subroutine make_desc_f(buf, desc)
   use mpi_f08_types
   use, intrinsic :: ISO_C_BINDING
   implicit none
   integer, target :: buf(:,:)
   type(CFI_cdesc_t), intent(inout) :: desc

   integer :: i, shp(2)

!   print *, "row1"
!   print *, buf(1,1:18)
!   print *, "col1"
!   print *, buf(1:18,1)
!   print *, "size=", size(buf)
!   print *, "shape=", shape(buf)
!   print *, "lb=", lbound(buf)
!   print *, "ub=", ubound(buf)

   shp = shape(buf)

   desc%base_addr = ompi_f08_addr(C_LOC(buf(1,1)))
   desc%elem_len = 4    ! C_SIZEOF(buf(1,1)) ?Intel compiler doesn't have this function?
   desc%rank = 2
   desc%type = 0;       ! no type info for now
   desc%attribute = 2;  ! assumed shape
   desc%state     = 1;  ! always 1 for assumed shape

   do i = 1, desc%rank
      desc%dim(i)%lower_bound = 1
      desc%dim(i)%extent = shp(i)
   end do

   desc%dim(1)%sm = ompi_f08_addr_diff(C_LOC(buf(1,1)), C_LOC(buf(2,1)))
   desc%dim(2)%sm = ompi_f08_addr_diff(C_LOC(buf(1,1)), C_LOC(buf(1,2)))
end subroutine

end module OMPI_Fortran_binding
