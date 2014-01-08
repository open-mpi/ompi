dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$

# Does this compiler support (void*)-like functionality for MPI choice
# buffers?  If so, which flavor?
AC_DEFUN([OMPI_FORTRAN_CHECK_IGNORE_TKR], [
    OPAL_VAR_SCOPE_PUSH([result happy type predecl])

    OMPI_FORTRAN_IGNORE_TKR_PREDECL=
    OMPI_FORTRAN_IGNORE_TKR_TYPE=

    AS_VAR_PUSHDEF([fortran_ignore_tkr_data], 
                   [ompi_cv_fortran_ignore_tkr_data])

    # Note that we can only cache 1 value at a time, but this test
    # needs to check for 2 things: the ignore TKR predecl and the
    # type.  So we encode them into a string of the form
    # <1|0>:<type>:<predecl>.  Ugh.
    AC_CACHE_CHECK([Fortran compiler ignore TKR syntax],
                    fortran_ignore_tkr_data,
                    [_OMPI_FORTRAN_CHECK_IGNORE_TKR])
    AS_VAR_COPY([result], [fortran_ignore_tkr_data])

    # Parse the result
    happy=`echo $result | cut -d: -f1`
    type=`echo $result | cut -d: -f2`
    predecl=`echo $result | cut -d: -f3-`

    AS_IF([test $happy -eq 1],
          [OMPI_FORTRAN_IGNORE_TKR_PREDECL=$predecl
           OMPI_FORTRAN_IGNORE_TKR_TYPE=$type
           $1],
          [$2])

    AS_VAR_POPDEF([fortran_ignore_tkr_data])
    OPAL_VAR_SCOPE_POP
])

################

AC_DEFUN([_OMPI_FORTRAN_CHECK_IGNORE_TKR], [
    OPAL_VAR_SCOPE_PUSH([happy ompi_fortran_ignore_tkr_predecl ompi_fortran_ignore_tkr_type])

    # If we were called here, it means that the value was not cached,
    # so we need to check several different things.  Since CACHE_CHECK
    # puts up a MSG_CHECKING, we need to terminate it with a bogus
    # answer before doing the individual checks.
    AC_MSG_RESULT([not cached; checking variants])

    # Default values
    ompi_fortran_ignore_tkr_predecl=!
    ompi_fortran_ignore_tkr_type=real

    # Vendor-neutral, TYPE(*) syntax
    OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB(
         [!], [type(*)],
         [TYPE(*), DIMENSION(*)],
         [happy=1], [happy=0])

    # GCC compilers
    AS_IF([test $happy -eq 0],
          [OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB(
              [!GCC\$ ATTRIBUTES NO_ARG_CHECK ::], [type(*), dimension(*)],
              [!GCC\$ ATTRIBUTES NO_ARG_CHECK],
              [happy=1], [happy=0])])
    # Intel compilers
    AS_IF([test $happy -eq 0],
          [OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB(
              [!DEC\$ ATTRIBUTES NO_ARG_CHECK ::], [real, dimension(*)],
              [!DEC\$ ATTRIBUTES NO_ARG_CHECK],
              [happy=1], [happy=0])])
    # Solaris Studio compilers
    # Note that due to a compiler bug, we have been advised by Oracle to 
    # use the "character(*)" type
    AS_IF([test $happy -eq 0],
          [OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB(
              [!\$PRAGMA IGNORE_TKR], [character(*)],
              [!\$PRAGMA IGNORE_TKR],
              [happy=1], [happy=0])])
    # Cray compilers
    AS_IF([test $happy -eq 0],
          [OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB(
              [!DIR\$ IGNORE_TKR], [real, dimension(*)],
              [!DIR\$ IGNORE_TKR],
              [happy=1], [happy=0])])
    # IBM compilers
    AS_IF([test $happy -eq 0],
          [OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB(
              [!IBM* IGNORE_TKR], [real, dimension(*)],
              [!IBM* IGNORE_TKR],
              [happy=1], [happy=0])])

    AS_VAR_SET(fortran_ignore_tkr_data, 
               [${happy}:${ompi_fortran_ignore_tkr_type}:${ompi_fortran_ignore_tkr_predecl}])

    # Now put the orignal CACHE_CHECK MSG_CHECKING back so that it can
    # output the MSG_RESULT.
    AC_MSG_CHECKING([Fortran compiler ignore TKR syntax])
    OPAL_VAR_SCOPE_POP
])dnl

###################################

# Generic check to see if Fortran compiler supports (void*)-like
# functionality
# $1: pre-decl qualifier line -- likely a compiler directive
# $2: parameter type
# $3: message for AC-MSG-CHECKING
# $4: action to take if the test passes
# $5: action to take if the test fails
AC_DEFUN([OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB], [
    OPAL_VAR_SCOPE_PUSH(msg)
    AC_LANG_PUSH([Fortran])
    AC_MSG_CHECKING([for Fortran compiler support of $3])
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([],[[!
! Autoconf puts "program main" at the top

  interface
     subroutine force_assumed_shape(a, count)
     integer :: count
     complex, dimension(:,:) :: a
     end subroutine force_assumed_shape
  end interface

  interface
     subroutine foo(buffer, count)
       $1 buffer
       $2, intent(in) :: buffer
       integer, intent(in) :: count
     end subroutine foo
  end interface

! Simple interface with an un-typed first argument (e.g., a choice buffer)
  integer :: count
  real :: buffer1(3)
  character :: buffer2
  complex :: buffer3(4,4)
  complex, pointer, dimension(:,:) :: ptr
  target :: buffer3
  ptr => buffer3  

! Set some known values (somewhat irrelevant for this test, but just be
! sure that the values are initialized)
  a = 17
  buffer1(1) = 4.5
  buffer1(2) = 6.7
  buffer1(3) = 8.9
  buffer2 = 'a'

! Call with one type for the first argument
  call foo(buffer1, count)
! Call with a different type for the first argument
  call foo(buffer2, count)
! Force us through an assumed shape
  call force_assumed_shape(buffer3, count)
! Force a pointer call through an assumed shape (!)
  ptr => buffer3  

  end program

  subroutine force_assumed_shape(a, count)
    integer :: count
    real, dimension(:,:) :: a
    call foo(a, count)
  end subroutine force_assumed_shape

! Autoconf puts "end" after the last line  
  subroutine bogus
]]),
                    [msg=yes
                     ompi_fortran_ignore_tkr_predecl="$1"
                     ompi_fortran_ignore_tkr_type="$2"
                     $4],
                    [msg=no
                     $5])
  AC_MSG_RESULT($msg)
  AC_LANG_POP([Fortran])
  OPAL_VAR_SCOPE_POP
])

