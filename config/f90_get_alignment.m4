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
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_F90_GET_ALIGNMENT(type, shell variable to set)
# ---------------------------------------------------
AC_DEFUN([OMPI_F90_GET_ALIGNMENT],[
    AS_VAR_PUSHDEF([type_var], [ompi_cv_f90_alignment_$1])

    AC_CACHE_CHECK([alignment of Fortran 90 $1], type_var,
       [OMPI_F77_MAKE_C_FUNCTION([ompi_ac_align_fn], [align])
        # Fortran module
        cat > conftestf.f90 <<EOF
program f90align
  type TestAlign
    character a
    $1 :: x
  end type
  external ALIGN
  type(TestAlign) :: a
  call ALIGN(a%a, a%x)
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
$conftest

#ifdef __cplusplus
extern "C" {
#endif
void $ompi_ac_align_fn(char *a, char *x);
void $ompi_ac_align_fn(char *a, char *x)
{   int diff;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);
    diff = a - x;
    fprintf(f, "%d\n", (diff >= 0) ? diff : -diff);
    fclose(f);
}
#ifdef __cplusplus
}
#endif
EOF

        OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
            [OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 conftestf.f90 conftest.o -o conftest $LDFLAGS $LIBS],
                [happy="yes"], [happy="no"])], [happy="no"])

        if test "$happy" = "no" ; then
             OMPI_LOG_MSG([here is the fortran 90 program:], 1)
             OMPI_LOG_FILE([conftestf.f90])
             AC_MSG_WARN([Could not determine alignment of $1])
             AC_MSG_WARN([See config.log for details])
             AC_MSG_ERROR([Cannot continue])
        fi

        AS_IF([test "$cross_compiling" = "yes"],
            [AC_MSG_ERROR([Can not determine alignment of $1 when cross-compiling])],
            [OMPI_LOG_COMMAND([./conftest],
                [AS_VAR_SET(type_var, [`cat conftestval`])],
                [AC_MSG_ERROR([Could not determine alignment of $1])])])

        unset happy ompi_conf
        rm -f conftest*])

    $2=AS_VAR_GET([type_var])
    AS_VAR_POPDEF([type_var])dnl
])
