! -*- f90 -*-
!
! Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
! Copyright (c) 2009-2012 Los Alamos National Security, LLC.
!               All Rights reserved.
! $COPYRIGHT$


   ! This wrapper mimics how the MPI_Recv_wrapper will eventually work.
   ! Eventually buf will be typed, TYPE(*), DIMENSION(..)
   ! Now can only mimic with explicit type and rank for assumed-shape dummy
   ! arguments.
   !
   subroutine MPI_Recv_f08_desc_int_2d(buf,count,datatype,source,tag,comm,status,ierror)
      use :: OMPI_Fortran_binding
      implicit none
      integer, INTENT(IN), target :: buf(:,:)
      INTEGER, INTENT(IN) :: count, source, tag
      TYPE(MPI_Datatype), INTENT(IN) :: datatype
      TYPE(MPI_Comm), INTENT(IN) :: comm
      TYPE(MPI_Status), INTENT(OUT) :: status
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      integer :: c_ierror
      type(CFI_cdesc_t) :: buf_desc

      call make_desc_f(buf, buf_desc) 
      !call print_desc(buf_desc)

      call ompi_recv_f08_desc_f(buf_desc, count, datatype%MPI_VAL, source, tag, comm%MPI_VAL, status, c_ierror)

      if (present(ierror)) ierror = c_ierror

   end subroutine MPI_Recv_f08_desc_int_2d


! WARNING, not yet implemented, stub used to test MPI_SUBARRAYS_SUPPORTED usage
!
   subroutine MPI_Recv_f08_desc_dbl_1d(buf,count,datatype,source,tag,comm,status,ierror)
      use :: OMPI_Fortran_binding
      implicit none
      double precision, INTENT(IN), target :: buf(:)
      INTEGER, INTENT(IN) :: count, source, tag
      TYPE(MPI_Datatype), INTENT(IN) :: datatype
      TYPE(MPI_Comm), INTENT(IN) :: comm
      TYPE(MPI_Status), INTENT(OUT) :: status
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      integer :: c_ierror

      ! this hack is to remove compiler warning about no change to out variable
      status = MPI_STATUS_IGNORE

      c_ierror = 1
      print *, "WARNING, testing of double precision arrays not yet supported with subarrays"

      if (present(ierror)) ierror = c_ierror

   end subroutine MPI_Recv_f08_desc_dbl_1d

! WARNING, not yet implemented, stub used to test MPI_SUBARRAYS_SUPPORTED usage
!
   subroutine MPI_Recv_f08_desc_dbl_0d(buf,count,datatype,source,tag,comm,status,ierror)
      use :: OMPI_Fortran_binding
      implicit none
      double precision, INTENT(IN), target :: buf
      INTEGER, INTENT(IN) :: count, source, tag
      TYPE(MPI_Datatype), INTENT(IN) :: datatype
      TYPE(MPI_Comm), INTENT(IN) :: comm
      TYPE(MPI_Status), INTENT(OUT) :: status
      INTEGER, OPTIONAL, INTENT(OUT) :: ierror

      integer :: c_ierror

      ! this hack is to remove compiler warning about no change to out variable
      status = MPI_STATUS_IGNORE

      c_ierror = 1
      print *, "WARNING, testing of double precision arrays not yet supported with subarrays"

      if (present(ierror)) ierror = c_ierror

   end subroutine MPI_Recv_f08_desc_dbl_0d

