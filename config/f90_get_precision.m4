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

AC_DEFUN([OMPI_F90_GET_PRECISION],[
# Determine precision of FORTRAN datatype.
# First arg is type, 2nd arg is config var to define.

AC_MSG_CHECKING(precision of FORTRAN $1)

cat > conftestf.f90 <<EOF
program main
    $1 :: x
    open(8, file="conftestval")
    write(8, fmt="(I5)") precision(x)
    close(8)
end program
EOF

#
# Try the compilation and run.
#

OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 -o conftest conftestf.f90],
	OMPI_LOG_COMMAND([./conftest], [HAPPY=1], [HAPPY=0]), [HAPPY=0])

ompi_ac_precision=-1
if test "$HAPPY" = "1" -a -f conftestval; then
    # get rid of leading spaces for eval assignment
    ompi_ac_precision=`sed 's/  *//' conftestval`
    AC_MSG_RESULT([$ompi_ac_precision])
    if test -n "$2"; then
	eval "$2=$ompi_ac_precision"
    fi
else
    AC_MSG_RESULT([unknown])

    OMPI_LOG_MSG([here is the fortran program:], 1)
    OMPI_LOG_FILE([conftestf.f90])

    AC_MSG_WARN([*** Problem running configure test!])
    AC_MSG_WARN([*** See config.log for details.])
    AC_MSG_ERROR([*** Cannot continue.])
fi
str="$2=$ompi_ac_precision"
eval $str

unset ompi_ac_precision HAPPY
/bin/rm -f conftest*])dnl
