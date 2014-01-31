dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# Special check for ticket 4157.  This test will eventually disappear.
# See https://svn.open-mpi.org/trac/ompi/ticket/4157.
#
# OMPI_FORTRAN_CHECK_TICKET_4157([action if happy], [action if not happy])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_TICKET_4157],[
    AS_VAR_PUSHDEF([ticket_4157_var], [ompi_cv_fortran_ticket_4157])

    AC_CACHE_CHECK([for ticket 4157 issues], ticket_4157_var,
       [AC_LANG_PUSH([Fortran])
        rm -rf conftest.$$.d
        mkdir conftest.$$.d
        cd conftest.$$.d
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
MODULE MY_ABSTRACT_MODULE
  ABSTRACT INTERFACE
     SUBROUTINE ABSTRACT_INTERFACE(A, B)
       IMPLICIT NONE
       INTEGER :: A
       LOGICAL :: B
     END SUBROUTINE ABSTRACT_INTERFACE
  END INTERFACE
END MODULE MY_ABSTRACT_MODULE  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE MY_CONCRETE_MODULE
  INTERFACE
     SUBROUTINE MY_CONCRETE_SUB(A, B)
       IMPLICIT NONE
       INTEGER :: A
       LOGICAL :: B
     END SUBROUTINE MY_CONCRETE_SUB
  END INTERFACE
END MODULE MY_CONCRETE_MODULE

SUBROUTINE MY_CONCRETE_SUB(A, B)
  IMPLICIT NONE
  INTEGER :: A
  LOGICAL :: B

  PRINT *, "I'm in MY_CONCRETE_SUB", A, B
END SUBROUTINE MY_CONCRETE_SUB

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE MY_MODULE
  INTERFACE
     SUBROUTINE MY_SUB(A, B) BIND(C, NAME="FOO")
       USE MY_ABSTRACT_MODULE
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: A
       PROCEDURE(ABSTRACT_INTERFACE) :: B
     END SUBROUTINE MY_SUB
  END INTERFACE
END MODULE MY_MODULE

SUBROUTINE MY_SUB(A, B) BIND(C, NAME="FOO")
  USE MY_ABSTRACT_MODULE
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: A
  PROCEDURE(ABSTRACT_INTERFACE) :: B

  LOGICAL :: C

  C = .TRUE.
  PRINT *, "I'm in MY_SUB", A, C
  CALL B(A, C)
END SUBROUTINE MY_SUB

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM TEST_ABSTRACT_PROCEDURE
  USE MY_ABSTRACT_MODULE
  USE MY_CONCRETE_MODULE
  USE MY_MODULE
  IMPLICIT NONE
  INTEGER :: FOO

  FOO = 3
  CALL MY_SUB(FOO, MY_CONCRETE_SUB)
END PROGRAM TEST_ABSTRACT_PROCEDURE
]])],
             [AS_VAR_SET(ticket_4157_var, happy)],
             [AS_VAR_SET(ticket_4157_var, unhappy)])
        AC_LANG_POP([Fortran])
        cd ..
        rm -rf contest.$$.d
       ])

    AS_VAR_IF(ticket_4157_var, [happy], [$1], [$2])
    AS_VAR_POPDEF([ticket_4157_var])
])
