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
dnl Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


# OMPI_FORTRAN_GET_VALUE_TRUE()
# -------------------------------------------------------
# Determine the value of .TRUE. of this Fortran compiler.
AC_DEFUN([OMPI_FORTRAN_GET_VALUE_TRUE],[
    # invalidate cache if result came from a run where FORTRAN was disabled
    if test "$ompi_cv_fortran_true_value" = "0" ; then
        unset ompi_cv_fortran_true_value
    fi

    AS_VAR_PUSHDEF([fortran_true_var], 
                   [ompi_cv_fortran_true_value])

    AC_CACHE_CHECK([Fortran value for .TRUE. logical type],
        fortran_true_var,
        [if test "$1" = "none" -o $OMPI_WANT_FORTRAN_BINDINGS -eq 0 -o $ompi_fortran_happy -eq 0 ; then
             value=77
         else
             #
             # C module
             # We really need the confdefs.h Header file for
             # the ompi_fortran_logical_t definition
             #
             if test \! -f confdefs.h ; then
                 AC_MSG_WARN([*** Problem running configure test!])
                 AC_MSG_WARN([*** Cannot find confdefs.h file for config test])
                 AC_MSG_WARN([*** See config.log for details.])
                 AC_MSG_ERROR([*** Cannot continue.])
             fi

             cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
#include "confdefs.h"

#ifdef __cplusplus
extern "C" {
#endif

void ompi_print_f(ompi_fortran_logical_t * logical)
{
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);

    if( SIZEOF_INT >= sizeof(ompi_fortran_logical_t) ) {
        fprintf(f, "%d\n", (int)*logical);
    } else if (SIZEOF_LONG >= sizeof(ompi_fortran_logical_t) ) {
	fprintf(f, "%ld\n", (long) *logical);
#ifdef HAVE_LONG_LONG
    } else if (SIZEOF_LONG_LONG >= sizeof(ompi_fortran_logical_t) ) {
        fprintf(f, "%lld\n", (long long) *logical);
#endif
    } else {
        exit(1);
    }
}

void ompi_print(ompi_fortran_logical_t *logical)
{ ompi_print_f(logical); }

void ompi_print_(ompi_fortran_logical_t *logical)
{ ompi_print_f(logical); }

void ompi_print__(ompi_fortran_logical_t *logical)
{ ompi_print_f(logical); }

void OMPI_PRINT(ompi_fortran_logical_t *logical)
{ ompi_print_f(logical); }

#ifdef __cplusplus
}
#endif
EOF
             cat > conftestf.f <<EOF
      program main
      logical value
      value=.TRUE.
      CALL ompi_print(value)
      end
EOF

             #
             # Try the compilation and run.
             #
             OPAL_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
                 [OPAL_LOG_COMMAND([$FC $FCFLAGS -o conftest conftest.o conftestf.f $LDFLAGS $LIBS],
                      [happy=1], [happy=0])],
                 [happy=0])

             AS_IF([test $happy -eq 0 -a $ompi_fortran_happy -eq 1],
                          [AC_MSG_ERROR([Could not compile Fortran .TRUE. test.  Aborting.])
                          ])

             AS_IF([test "$cross_compiling" = "yes"],
                 [AC_MSG_ERROR([Can not determine value of .TRUE. when cross-compiling])],
                 [OPAL_LOG_COMMAND([./conftest],
                     [value=`sed 's/  *//' conftestval`],
                     [AC_MSG_ERROR([Could not determine value of Fotran .TRUE..  Aborting.])])])
         fi
         AS_VAR_SET(fortran_true_var, [$value])
         unset value
        ])

    AS_VAR_COPY([ompi_fortran_true_value], [fortran_true_var])
    AC_DEFINE_UNQUOTED([OMPI_FORTRAN_VALUE_TRUE], 
        [$ompi_fortran_true_value],
        [Fortran value for LOGICAL .TRUE. value])
    AS_VAR_POPDEF([fortran_true_var])

    unset happy ompi_print_logical_fn
    rm -rf conftest*
])dnl
