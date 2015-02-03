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
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_FORTRAN_CHECK_LOGICAL_ARRAY],[
    AS_VAR_PUSHDEF([logical_array_var], 
                   [ompi_cv_fortran_logical_array_correct])

    AC_CACHE_CHECK([for correct handling of Fortran logical arrays],
        logical_array_var,
        [if test "$1" = "none" || test $OMPI_WANT_FORTRAN_BINDINGS -eq 0 || test $ompi_fortran_happy -eq 0; then
             value=skipped
         else
             # Fortran module
             cat > conftestf.f <<EOF
        program check_logical_array
        external ompi_check
        logical l(2)
        l(1)=.FALSE.
        l(2)=.TRUE.
        CALL ompi_check(l)
        end
EOF

             # C module
             # We really need the confdefs.h Header file for 
             # the ompi_fortran_logical_t definition
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
void ompi_check_f(ompi_fortran_logical_t * logical)
{
    int result = 0;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);

    if (logical[[0]] == 0 &&
        logical[[1]] == $ompi_cv_fortran_true_value)
      result = 1;
    fprintf(f, "%d\n", result);
}

void ompi_check(ompi_fortran_logical_t * logical)
{ ompi_check_f(logical); }

void ompi_check_(ompi_fortran_logical_t * logical)
{ ompi_check_f(logical); }

void ompi_check__(ompi_fortran_logical_t * logical)
{ ompi_check_f(logical); }

void OMPI_CHECK(ompi_fortran_logical_t * logical)
{ ompi_check_f(logical); }

#ifdef __cplusplus
}
#endif
EOF

             # Try the compilation and run.  Can't use AC_TRY_RUN
             # because it's two module files.
             OPAL_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
                 [OPAL_LOG_COMMAND([$FC $FCFLAGS conftestf.f conftest.o -o conftest $LDFLAGS $LIBS],
                     [happy=1], [happy=0])],
                 [happy=0])
             if test "$happy" = "0" ; then
                 AC_MSG_ERROR([Error determining if arrays of logical values work properly.])
             fi

             AS_IF([test "$cross_compiling" = "yes"], 
                 [ # assume we're ok
                  value=yes],
                 [OPAL_LOG_COMMAND([./conftest],
                      [if test "`cat conftestval`" = "1" ; then
                           value=yes
                       else
                           value=no
                       fi],             
                      [value=no])])
         fi
         AS_VAR_SET(logical_array_var, [$value])
         ])

    AS_VAR_COPY([ompi_fortran_logical_array_correct], [logical_array_var])
    if test "$ompi_fortran_logical_array_correct" = "no" ; then
        AC_MSG_ERROR([Error determining if arrays of logical values work properly.])
    fi
    AS_VAR_POPDEF([logical_array_var])

    unset happy ompi_check_logical_fn
    rm -rf conftest*
])dnl
