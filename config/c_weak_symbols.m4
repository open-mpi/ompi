dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

define([OMPI_C_WEAK_SYMBOLS],[
#
# Arguments: None
#
# Depdencies: None
#
# Check to see if the C compiler can handle weak symbols
#
# Sets OMPI_C_WEAK_SYMBOLS=1 if the C compiler has support for weak
# symbols.
#

AC_MSG_CHECKING([for weak symbols])
AC_LINK_IFELSE(AC_LANG_SOURCE([[#pragma weak fake = real
extern int fake(int i);
int real(int i);
int real(int i) { return i; }
int main(int argc, char* argv[]) {
  return fake(3);
}]]), ompi_happy=1, ompi_happy=0)
if test "$ompi_happy" = "1"; then
    AC_MSG_RESULT([yes])
    OMPI_C_HAVE_WEAK_SYMBOLS=1
else
    AC_MSG_RESULT([no])
    OMPI_C_HAVE_WEAK_SYMBOLS=0
fi

unset ompi_happy])dnl
