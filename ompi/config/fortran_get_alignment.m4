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

# This is a long, sordid tale.

# We have been unable to devise a F90 test that will result in a
# consistent alignment answer.  Specifically, our prior tests have
# been similar to the prior f77 test -- have a small chunk of f90 code
# compiled with the C code to actually compute the offsets.  The f90
# code was a struct-like entity (a "type") with multiple members -- on
# a character and the other of the target type.  The C code measured
# the distance between them.  But even if you use the keyword to
# ensure that the F90 compiler does not re-order this struct, you may
# still get a different alignment answer than the F77 test (!).  This
# is apparently because F90 allows compilers to align types
# differently according to use (in common blocks, as standalone
# variables, and as a member of a struct).  Hence, the alignment can
# be different depending on how to measure (and use) it.  This was
# confirmed by various members of the Fortran committee and several
# Fortran compiler vendors.

# We use to check the F77 alignment based on common block usage, but
# this is only one of the available types for F90.  Hence, we may
# actually get a different answer between f77 and f90 in the same
# compiler series (and some compilers do!  E.g., g95 gives different
# answers even when "g95" itself is used as both the f77 and f90
# compiler).

# So we gave up -- the only thing we can do (and has worked for years)
# is to check Fortran alignment in common blocks when compiled with .f
# files (not .f90).

# Indeed, just because data is coming from the mpif.h bindings doesn't
# mean it wasn't compiled with the f90 (or later) compiler.  So
# there's no way to tell -- just hope that common block alignment is
# good enough.  :-(

# OMPI_FORTRAN_GET_ALIGNMENT(type, shell variable to set)
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_GET_ALIGNMENT],[
    unset happy
    OPAL_VAR_SCOPE_PUSH([happy ompi_conftest_h])
    # Use of m4_translit suggested by Eric Blake:
    # http://lists.gnu.org/archive/html/bug-autoconf/2010-10/msg00016.html
    AS_VAR_PUSHDEF([type_var],
       m4_translit([[ompi_cv_fortran_alignment_$1]], [*], [p]))

    AC_CACHE_CHECK([alignment of Fortran $1], type_var,
       [OMPI_FORTRAN_MAKE_C_FUNCTION([ompi_ac_align_fn], [align])
        # Fortran module.  Make sure it's .f, not .f90.
        cat > conftestf.f <<EOF
      program falign
      external align
      $1  w,x,y,z
      CHARACTER a,b,c
      common /foo/a,w,b,x,y,c,z
      call align(w,x,y,z)
      end
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
void $ompi_ac_align_fn(char *w, char *x, char *y, char *z)
{   unsigned long aw, ax, ay, az;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);
    aw = (unsigned long) w;
    ax = (unsigned long) x;
    ay = (unsigned long) y;
    az = (unsigned long) z;
    if (! ((aw%16)||(ax%16)||(ay%16)||(az%16))) fprintf(f, "%d\n", 16);
    else if (! ((aw%12)||(ax%12)||(ay%12)||(az%12))) fprintf(f, "%d\n", 12);
    else if (! ((aw%8)||(ax%8)||(ay%8)||(az%8))) fprintf(f, "%d\n", 8);
    else if (! ((aw%4)||(ax%4)||(ay%4)||(az%4))) fprintf(f, "%d\n", 4);
    else if (! ((aw%2)||(ax%2)||(ay%2)||(az%2))) fprintf(f, "%d\n", 2);
    else fprintf(f, "%d\n", 1); 
    fclose(f);
}
#ifdef __cplusplus
}
#endif
EOF

        OPAL_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
            [OPAL_LOG_COMMAND([$FC $FCFLAGS conftestf.f conftest.o -o conftest $LDFLAGS $LIBS],
                [happy="yes"], [happy="no"])], [happy="no"])

        if test "$happy" = "no" ; then
            AC_MSG_RESULT([Error!])
            AC_MSG_ERROR([Could not determine alignment of $1])
        fi

        AS_IF([test "$cross_compiling" = "yes"],
            [AC_MSG_RESULT([Error!])
             AC_MSG_ERROR([Can not determine alignment of $1 when cross-compiling])],
            [OPAL_LOG_COMMAND([./conftest],
                [AS_VAR_SET(type_var, [`cat conftestval`])],
                [AC_MSG_RESULT([Error!])
                 AC_MSG_ERROR([Could not determine alignment of $1])])])
        rm -rf conftest*])

    AS_VAR_COPY([$2], [type_var])
    AS_VAR_POPDEF([type_var])dnl
    OPAL_VAR_SCOPE_POP
])
