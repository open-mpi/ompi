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

AC_DEFUN([OMPI_F77_GET_SIZEOF],[
# Determine FORTRAN datatype size.
# First arg is type, 2nd arg is config var to define.
AC_MSG_CHECKING([size of FORTRAN $1])
ompi_ac_size_fn=
if test "x$ompi_ac_doubleunder" = xy || test "x$ompi_ac_singleunder" = xy; then
    ompi_ac_size_fn=size_
else
    if test "x$ompi_ac_nounder" = xy; then
	    ompi_ac_size_fn=size
    else 
	if test "x$ompi_ac_caps" = xy; then
	    ompi_ac_size_fn=SIZE
	else
	    AC_MSG_WARN([*** FORTRAN external naming convention undefined])
	    AC_MSG_ERROR([*** Cannot continue.])
	fi
    fi
fi

#
# Cannot use standard AC_TRY macros because we need two different .o
# files here, and link them together
#

#
# Fortran module
#

cat > conftestf.f <<EOF
       program fsize
       external SIZE
       $1 x(2)
       call SIZE(x(1),x(2))
       end
EOF

#
# C module
#

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

#
# Try the compilation and run.  Can't use AC_TRY_RUN because it's two
# module files.
#

OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
    OMPI_LOG_COMMAND([$F77 $FFLAGS conftestf.f conftest.o -o conftest],
	OMPI_LOG_COMMAND([./conftest],[HAPPY=1],[HAPPY=0]),
	[HAPPY=0]),
    [HAPPY=0])

ompi_ac_fortsize=-1
if test "$HAPPY" = "1" -a -f conftestval; then
    ompi_ac_fortsize=`cat conftestval`
    AC_MSG_RESULT([$ompi_ac_fortsize])
    eval "$2=$ompi_ac_fortsize"
else
    AC_MSG_RESULT([unknown])

    OMPI_LOG_MSG([here is the C program:], 1)
    OMPI_LOG_FILE([conftest.c])
    if test -f conftest.h; then
	OMPI_LOG_MSG([here is contest.h:], 1)
	OMPI_LOG_FILE([conftest.h])
    fi
    OMPI_LOG_MSG([here is the fortran program:], 1)
    OMPI_LOG_FILE([conftestf.f])

    AC_MSG_WARN([*** Problem running configure test!])
    AC_MSG_WARN([*** See config.log for details.])
    AC_MSG_ERROR([*** Cannot continue.])
fi
str="$2=$ompi_ac_fortsize"
eval $str

unset ompi_ac_fortsize HAPPY ompi_conftest_h
/bin/rm -f conftest*])dnl
