dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

define([LAM_CXX_HAVE_EXCEPTIONS],[
#
# Arguments: None
#
# Depdencies: None
#
# Check to see if the C++ compiler can handle exceptions 
#
# Sets LAM_CXX_EXCEPTIONS to 1 if compiler has exceptions, 0 if not
#

AC_MSG_CHECKING([for throw/catch])
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_COMPILE(, int i=1; throw(i);, 
    LAM_CXX_EXCEPTIONS=1, LAM_CXX_EXCPTIONS=0)
if test "$LAM_CXX_EXCEPTIONS" = "1"; then
    AC_MSG_RESULT([yes])
else
    AC_MSG_RESULT([no])
fi

# Clean up
AC_LANG_RESTORE])dnl
