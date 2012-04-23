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
dnl Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# Check whether or not the Fortran compiler supports optional
# arguments or not, and we (generally) don't need wrapper subroutines.

# OMPI_FORTRAN_CHECK_OPTIONAL_ARGS([action if found], 
#                                  [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_OPTIONAL_ARGS],[
    unset happy
    OPAL_VAR_SCOPE_PUSH([happy ompi_conftest_h])

    AC_CACHE_CHECK([if Fortran compiler supports optional arguments], 
       [ompi_cv_fortran_optional_args],
       [ompi_cv_fortran_optional_args=no
        OMPI_FORTRAN_MAKE_C_FUNCTION([ompi_ac_check_op_fn], [check_op])
        cat > conftestf.f90 <<EOF
program check_for_optional
   use, intrinsic :: iso_c_binding

   interface
      subroutine check_op(i, ierror)
         integer, intent(in), value :: i
         integer, intent(out), optional :: ierror
      end subroutine check_op
   end interface

   integer :: ierror

   call check_op(0)
   call check_op(1, ierror)
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
void $ompi_ac_check_op_fn(int has_op, int *ierror)
{
    /* Force a segv if the conditions are wrong */
    char *bogus = 0;
    if (0 == has_op) {
        /* won't have optional argument */
        if (NULL != ierror) *bogus= 13;
   } else {
        /* will have optional argument */
        if (NULL == ierror) *bogus= 13;
        *ierror = 33;
   }
}
#ifdef __cplusplus
}
#endif
EOF

        OPAL_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
            [OPAL_LOG_COMMAND([$FC $FCFLAGS conftestf.f90 conftest.o -o conftest $LDFLAGS $LIBS],
                [happy="yes"], [happy="no"])], 
            [happy="c_fail"])

        AS_IF([test "$happy" = "c_fail"],
              [AC_MSG_RESULT([error])
               AC_MSG_ERROR([This error should not happen -- contact the Open MPI developers])
              ])

        AS_IF([test "$happy" = "no"],
              [AC_MSG_RESULT([unknown])
               AC_MSG_WARN([Cannot determine if Fortran compiler supports optional arguments])
               AC_MSG_WARN([Assuming: no])
              ])

        AS_IF([test "$cross_compiling" = "yes"],
            [AC_MSG_RESULT([cross-compiling])
             AC_MSG_ERROR([Cannot determine if Fortran compiler supports optional arguments when cross-compiling])],
            [OPAL_LOG_COMMAND([./conftest],
                [ompi_cv_fortran_optional_args=yes])
            ])
        rm -f conftest*
    ])dnl

    AS_VAR_COPY([happy], [ompi_cv_fortran_optional_args])
    AS_IF([test "$happy" = "yes"], [$1], [$2])
    OPAL_VAR_SCOPE_POP
])
