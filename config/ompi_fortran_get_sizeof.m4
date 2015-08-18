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
dnl Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_FORTRAN_GET_SIZEOF(prologue, type, variable to set)
# ------------------------------------------
AC_DEFUN([OMPI_FORTRAN_GET_SIZEOF],[
    # Use of m4_translit suggested by Eric Blake:
    # http://lists.gnu.org/archive/html/bug-autoconf/2010-10/msg00016.html
    AS_VAR_PUSHDEF([type_var],
       m4_translit([[ompi_cv_fortran_sizeof_$2]], [*], [p]))

    AC_CACHE_CHECK([size of Fortran $2], type_var,
        [OMPI_FORTRAN_MAKE_C_FUNCTION([ompi_ac_size_fn], [size])
         # Fortran module
         cat > conftestf.f90 <<EOF
program fsize
$1
   external size
   $2 :: x(2)
   call size(x(1),x(2))
end program
EOF

         # C module
         if test -f conftest.h; then
             ompi_conftest_h="#include \"conftest.h\""
         else
             ompi_conftest_h=""
         fi
         cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
$ompi_conftest_h

#ifdef __cplusplus
extern "C" {
#endif
void $ompi_ac_size_fn(char *a, char *b)
{
    int diff = (int) (b - a);
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);
    fprintf(f, "%d\n", diff);
}
#ifdef __cplusplus
}
#endif
EOF

         OPAL_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
             [OPAL_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 conftestf.f90 conftest.o -o conftest $LDFLAGS $LIBS],
                  [happy="yes"], [happy="no"])], [happy="no"])

         if test "$happy" = "no" ; then
             OPAL_LOG_MSG([here is the Fortran program:], 1)
             OPAL_LOG_FILE([conftestf.f90])
             AC_MSG_WARN([Could not determine size of $2])
             AC_MSG_WARN([See config.log for details])
             AC_MSG_ERROR([Cannot continue])
         fi

         AS_IF([test "$cross_compiling" = "yes"],
             [AC_MSG_ERROR([Can not determine size of $2 when cross-compiling])],
             [OPAL_LOG_COMMAND([./conftest],
                 [AS_VAR_SET(type_var, [`cat conftestval`])],
                 [AC_MSG_ERROR([Could not determine size of $2])])])

        unset happy ompi_conftest_h
        rm -rf conftest*])

    AS_VAR_COPY([$3], [type_var])
    AS_VAR_POPDEF([type_var])dnl
])dnl
