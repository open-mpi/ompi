dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_F77_CHECK_TYPE],[
# Determine FORTRAN datatype size.
# First arg is type, 2nd arg is config var to define

AC_MSG_CHECKING([if FORTRAN compiler supports $1])

AC_LANG_PUSH(Fortran 77)
AC_COMPILE_IFELSE(AC_LANG_SOURCE([[C
        program main
        $1 bogus_variable
        end]]), 
    [HAPPY=1 
     AC_MSG_RESULT([yes])],
    [HAPPY=0 
     AC_MSG_RESULT([no])])
AC_LANG_POP

str="$2=$HAPPY"
echo happy is: $HAPPY
echo str is: $str
eval $str

unset HAPPY])dnl
