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
dnl Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_F77_CHECK_REAL16_C_EQUIV
# ----------------------------------------------------
AC_DEFUN([OMPI_F77_CHECK_REAL16_C_EQUIV],[
    unset happy
    OMPI_VAR_SCOPE_PUSH([happy define_value msg])
    AS_VAR_PUSHDEF([real16_matches_c_var], [ompi_cv_real16_c_equiv])

    msg="in C"
    AS_IF([test "$OMPI_WANT_F77_BINDINGS" = "1"],
          [AS_IF([test "$OMPI_HAVE_FORTRAN_REAL16" = "1"],
                 [msg="of $OMPI_FORTRAN_REAL16_C_TYPE"])])
    AC_CACHE_CHECK([if REAL*16 matches bit representation $msg], 
        real16_matches_c_var,
        [AS_IF([test "$OMPI_WANT_F77_BINDINGS" = "1" -a "$OMPI_HAVE_FORTRAN_REAL16" = "1"],[
            # Make a C function of the Right name
            OMPI_F77_MAKE_C_FUNCTION([ompi_ac_c_fn], [c])

            # C module
            cat > conftest_c.c <<EOF
#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif
void $ompi_ac_c_fn($OMPI_FORTRAN_REAL16_C_TYPE *a) {
    FILE *fp = fopen("conftestval", "w");
    if (NULL == fp) exit(1);
    fprintf(fp, "%s\n", (1.1L == *a) ? "yes" : "no");
    fclose(fp);
}
#ifdef __cplusplus
}
#endif
EOF

            # Fortran module
            cat > conftest_f.f <<EOF
        program bogus
        REAL*16 :: foo
        foo = 1.1
        call c(foo)
        end program bogus
EOF
            rm -f conftestval
            # Compile and link
            OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest_c.c],
                [OMPI_LOG_COMMAND([$F77 $FFLAGS conftest_f.f conftest_c.o -o conftest $LDFLAGS $LIBS],
                    [happy="yes"], [happy="no"])], [happy="no"])
            AS_IF([test "$happy" = "no"],
                [AC_MSG_RESULT([Error!])
                 AC_MSG_ERROR([Could not determine if REAL*16 bit-matches C type])])

            # If it worked so far, try running to see what we get
            AS_IF([test "$cross_compiling" = "yes"],
                [AC_MSG_RESULT([Error!])
                 AC_MSG_ERROR([Can not determine if REAL*16 bit-matches C if cross compiling])],
                [OMPI_LOG_COMMAND([./conftest],
                    [AS_VAR_SET(real16_matches_c_var, [`cat conftestval`])],
                    [AC_MSG_RESULT([Error!])
                     AC_MSG_ERROR([Could not determine if REAL*16 bit-matches C type])
                    ])
                ])

            # All done; whack tmp files
            rm -rf conftest*
        ],[
            # No fortran bindings or no REAL*16
            AS_IF([test "$OMPI_WANT_F77_BINDINGS" = "0"],
                  [msg="no (no Fortran bindings)"],
                  [msg="no (no REAL*16)"])
            AS_VAR_SET(real16_matches_c_var, [$msg])
        ])
    ])

    AS_VAR_COPY([ompi_real16_matches_c], [real16_matches_c_var])
    AS_VAR_POPDEF([real16_matches_c_var])

    AS_IF([test "$ompi_real16_matches_c" = "yes"],
          [define_value=1], 
          [define_value=0
           AC_MSG_WARN([MPI_REAL16 and MPI_COMPLEX32 support have been disabled])])
   AC_DEFINE_UNQUOTED([OMPI_REAL16_MATCHES_C], [$define_value],
                      [Whether Fortran REAL*16 matches the bit format of the equivalent C type])
    OMPI_VAR_SCOPE_POP
])
