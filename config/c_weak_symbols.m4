dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_C_WEAK_SYMBOLS()
# ---------------------
# sets OMPI_C_WEAK_SYMBOLS=1 if C compiler has support for weak symbols
define([OMPI_C_WEAK_SYMBOLS],[
    AC_CACHE_CHECK([for weak symbols],
                    [ompi_cv_c_weak_symbols],
                    [AC_LINK_IFELSE([AC_LANG_SOURCE([[#pragma weak fake = real
extern int fake(int i);
int real(int i);
int real(int i) { return i; }
int main(int argc, char* argv[]) {
  return fake(3);
}]])],
                                    [ompi_cv_c_weak_symbols="yes"],
                                    [ompi_cv_c_weak_symbols="no"])])

    AS_IF([test "$ompi_cv_c_weak_symbols" = "yes"],
          [OMPI_C_HAVE_WEAK_SYMBOLS=1], [OMPI_C_HAVE_WEAK_SYMBOLS=0])
]) dnl
