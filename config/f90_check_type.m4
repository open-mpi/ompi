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

AC_DEFUN([OMPI_F90_CHECK_TYPE],[
# Determine FORTRAN datatype size.
# First arg is type, 2nd arg is config var to define

AC_MSG_CHECKING([if FORTRAN compiler supports $1])

cat > conftestf.f90 <<EOF
program main
   if ($1 .lt. 0) stop 1
end program
EOF

#
# Try the compilation and run.
#

OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 -o conftest conftestf.f90],
	OMPI_LOG_COMMAND([./conftest],
	                 [HAPPY=1
                          AC_MSG_RESULT([yes])],
                         [HAPPY=0
                          AC_MSG_RESULT([no])]), [HAPPY=0
                                                  AC_MSG_RESULT([no])])

str="$2=$HAPPY"
eval $str

unset HAPPY])dnl
