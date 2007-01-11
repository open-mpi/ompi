dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CHECK_BOOL_STRUCT_COPY],[
dnl
dnl Portland PGI 6.2.x will choke when compiling a C code that copies
dnl a struct by value that contains bool members.  This bug was fixed in
dnl PGI 7.0.
dnl
AC_MSG_CHECKING([whether structs containing bools can be copied by value])
AC_TRY_COMPILE([
/* If we do not have bool, just provide some test that will trivially
   compile (because OMPI will set its own type for bool later) */
#if !defined(SIZEOF_BOOL)
int foo(void) { return 1; }
#else
#if OMPI_USE_STDBOOL_H
#include <stdbool.h>
#endif

struct bar_t {
    bool a;
};

void foo(void) {
    struct bar_t c, d;
    c = d; /* this is where the error occurs */
}
#endif
],[],[ompi_ac_sbc=1 ompi_ac_sbc_msg=yes],[ompi_ac_sbc=0 ompi_ac_sbc_msg=no])

AC_DEFINE_UNQUOTED([OPAL_BOOL_STRUCT_COPY], [$ompi_ac_sbc],
    [Whether the compiler supports copying structs by value that contain bool members (PGI 6.2.x had a bug such that this would choke the compiler)])
AC_MSG_RESULT([$ompi_ac_sbc_msg])])dnl
