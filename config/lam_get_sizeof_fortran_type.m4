dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2001-2002 The Trustees of Indiana University.  
dnl                         All rights reserved.
dnl Copyright (c) 1998-2001 University of Notre Dame. 
dnl                         All rights reserved.
dnl Copyright (c) 1994-1998 The Ohio State University.  
dnl                         All rights reserved.
dnl 
dnl This file is part of the LAM/MPI software package.  For license
dnl information, see the LICENSE file in the top level directory of the
dnl LAM/MPI source distribution.
dnl
dnl $Id: lam_get_sizeof_fortran_type.m4,v 1.1 2004/01/16 22:16:25 pkambadu Exp $
dnl

define(LAM_GET_SIZEOF_FORTRAN_TYPE,[
# Determine FORTRAN datatype size.
# First arg is type, 2nd (optional) arg is config var to define.
AC_MSG_CHECKING([size of FORTRAN $1])
lam_ac_size_fn=
if test "x$lam_ac_doubleunder" = xy || test "x$lam_ac_singleunder" = xy; then
    lam_ac_size_fn=size_
else
    if test "x$lam_ac_nounder" = xy; then
	    lam_ac_size_fn=size
    else 
	if test "x$lam_ac_caps" = xy; then
	    lam_ac_size_fn=SIZE
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
    lam_conftest_h="#include \"conftest.h\""
else
    lam_conftest_h=""
fi
cat > conftest.c <<EOF
#include <stdio.h>
#include <stdlib.h>
$lam_conftest_h

#ifdef __cplusplus
extern "C" {
#endif
void $lam_ac_size_fn(char *a, char *b)
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

LAM_LOG_COMMAND([$CC $CFLAGS -I. -c conftest.c],
    LAM_LOG_COMMAND([$F77 $FFLAGS conftestf.f conftest.o -o conftest],
	LAM_LOG_COMMAND([./conftest],[HAPPY=1],[HAPPY=0]),
	[HAPPY=0]),
    [HAPPY=0])

if test "$HAPPY" = "1" -a -f conftestval; then
    lam_ac_fortsize=`cat conftestval`
    AC_MSG_RESULT([$lam_ac_fortsize])
    if test -n "$2"; then
	eval "$2=$lam_ac_fortsize"
    fi
else
    AC_MSG_RESULT([unknown])

    LAM_LOG_MSG([here is the C program:], 1)
    LAM_LOG_FILE([conftest.c])
    if test -f conftest.h; then
	LAM_LOG_MSG([here is contest.h:], 1)
	LAM_LOG_FILE([conftest.h])
    fi
    LAM_LOG_MSG([here is the fortran program:], 1)
    LAM_LOG_FILE([conftestf.f])

    AC_MSG_WARN([*** Problem running configure test!])
    AC_MSG_WARN([*** See config.log for details.])
    AC_MSG_ERROR([*** Cannot continue.])
fi

unset lam_ac_fortsize HAPPY lam_conftest_h
/bin/rm -f conftest*])dnl
