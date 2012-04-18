! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$

   ! This wrapper mimics how the MPI_Send_wrapper will eventually work.
   ! Eventually buf will be typed, TYPE(*), DIMENSION(..)
   ! Now can only mimic with explicit type and rank for assumed-shape dummy
   ! arguments.
   !
   subroutine MPI_Send_f08_desc_int_2d(buf, count, datatype, dest, tag, comm, ierror)
      use OMPI_Fortran_binding
      implicit none
      integer,            intent(in), target :: buf(:,:)
      integer,            intent(in) :: count, dest, tag
      type(MPI_Datatype), intent(in) :: datatype
      type(MPI_Comm),     intent(in) :: comm
      integer, optional,  intent(out) :: ierror
  
      integer :: err
      type(CFI_cdesc_t) :: buf_desc

      call make_desc_f(buf, buf_desc)
      !call print_desc(buf_desc)

      call ompi_send_f08_desc_f(buf_desc, count, datatype%MPI_VAL, dest, tag, comm%MPI_VAL, err)

      if (present(ierror)) ierror = err

   end subroutine MPI_Send_f08_desc_int_2d


! WARNING, not yet implemented, stub used to test MPI_SUBARRAYS_SUPPORTED usage
!
   subroutine MPI_Send_f08_desc_dbl_1d(buf, count, datatype, dest, tag, comm, ierror)
      use OMPI_Fortran_binding
      implicit none
      double precision,   intent(in), target :: buf(:)
      integer,            intent(in) :: count, dest, tag
      type(MPI_Datatype), intent(in) :: datatype
      type(MPI_Comm),     intent(in) :: comm
      integer, optional,  intent(out) :: ierror
  
      integer :: err

      print *, "WARNING, testing of double precision arrays not yet supported with subarrays"
      err = 1

      if (present(ierror)) ierror = err

   end subroutine MPI_Send_f08_desc_dbl_1d

! WARNING, not yet implemented, stub used to test MPI_SUBARRAYS_SUPPORTED usage
!
   subroutine MPI_Send_f08_desc_dbl_0d(buf, count, datatype, dest, tag, comm, ierror)
      use OMPI_Fortran_binding
      implicit none
      double precision,   intent(in), target :: buf
      integer,            intent(in) :: count, dest, tag
      type(MPI_Datatype), intent(in) :: datatype
      type(MPI_Comm),     intent(in) :: comm
      integer, optional,  intent(out) :: ierror
  
      integer :: err

      print *, "WARNING, testing of double precision arrays not yet supported with subarrays"
      err = 1

      if (present(ierror)) ierror = err

   end subroutine MPI_Send_f08_desc_dbl_0d
