dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_F90_GET_ALIGNMENT],[
# Determine FORTRAN datatype size.
# First arg is type, 2nd arg is config var to define.
AC_MSG_CHECKING(alignment of FORTRAN $1)
ompi_ac_align_fn=
if test "x$ompi_ac_doubleunder" = xy || test "x$ompi_ac_singleunder" = xy; then
    ompi_ac_align_fn=align_
else
    if test "x$ompi_ac_nounder" = xy; then
	ompi_ac_align_fn=align
    else
	if test "x$ompi_ac_caps" = xy; then
	    ompi_ac_align_fn=ALIGN
	else
	    AC_MSG_WARN([*** FORTRAN external naming convention undefined])
	    AC_MSG_ERROR([*** Cannot continue.])
	fi
    fi
fi

#
# Fortran module
#

cat > conftestf.f90 <<EOF
module OMPI_TEST_ALIGN
  type TestAlign
    character a
    $1 :: x
  end type
end module

program f90align
  use OMPI_TEST_ALIGN
  external ALIGN
  type(TestAlign) :: a
  call ALIGN(a%a, a%x)
end program
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
$conftest

#ifdef __cplusplus
extern "C" {
#endif
void $ompi_ac_align_fn(char *a, char *x)
{
    int diff;
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

#
# Try the compilation and run.  Can't use AC_TRY_RUN because it's two
# module files.
#

OMPI_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
    OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 conftestf.f90 conftest.o -o conftest],
	OMPI_LOG_COMMAND([./conftest],[HAPPY=1],[HAPPY=0]),
	[HAPPY=0]),
    [HAPPY=0])

if test "$HAPPY" = "1" -a -f conftestval; then
    ompi_ac_align=`cat conftestval`
    AC_MSG_RESULT([$ompi_ac_align])
    if test -n "$2"; then
	eval "$2=$ompi_ac_align"
    fi
else
    AC_MSG_RESULT([unknown])

    OMPI_LOG_MSG([here is the C program:], 1)
    OMPI_LOG_FILE([conftest.c])
    if test -f conftest.h; then
	OMPI_LOG_MSG([here is contest.h:], 1)
	OMPI_LOG_FILE([conftest.h])
    fi
    OMPI_LOG_MSG([here is the fortran program:], 1)
    OMPI_LOG_FILE([conftestf.f90])

    AC_MSG_WARN([*** Problem running configure test!])
    AC_MSG_WARN([*** See config.log for details.])
    AC_MSG_ERROR([*** Cannot continue.])
fi
str="$2=$ompi_ac_align"
eval $str

unset ompi_ac_align HAPPY ompi_conftest_h
/bin/rm -f conftest*])dnl
