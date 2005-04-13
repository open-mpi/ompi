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

AC_DEFUN([OMPI_F90_FIND_MODULE_INCLUDE_FLAG],[

AC_MSG_CHECKING([for FORTRAN compiler module include flag])
possible_flags="-I -p -M"

mkdir conftest.$$
cd conftest.$$

#
# Try to compile an F90 module
#

mkdir subdir
cd subdir
cat > conftest-module.f90 <<EOF
module OMPI_MOD_FLAG

  type OMPI_MOD_FLAG_TYPE
    integer :: i
  end type OMPI_MOD_FLAG_TYPE

end module OMPI_MOD_FLAG
EOF

OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 -c conftest-module.f90], ,
                 AC_MSG_RESULT([Whoops!])
                 AC_MSG_WARN([*** Cannot seem to compile an f90 module])
                 AC_MSG_ERROR([Cannot continue]))
cd ..

#
# Now try to compile a simple program usinng that module, iterating
# through the possible flags that the compiler might use
#

cat > conftest.f90 <<EOF
program main
  use OMPI_MOD_FLAG
end program main
EOF

OMPI_FC_MODULE_FLAG=
for flag in $possible_flags; do
    if test "$OMPI_FC_MODULE_FLAG" = ""; then
        OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 conftest.f90 $flag subdir],
                        [OMPI_FC_MODULE_FLAG="$flag"])
    fi
done
cd ..
rm -rf conftest.$$

#
# Did we find it?
#

if test "$OMPI_FC_MODULE_FLAG" = ""; then
    AC_MSG_RESULT([])
    AC_MSG_WARN([*** Could not determine the f90 compiler flag to indicate where modules reside])
    AC_MSG_ERROR([Cannot continue])
fi
AC_MSG_RESULT([$OMPI_FC_MODULE_FLAG])

AC_SUBST(OMPI_FC_MODULE_FLAG)])dnl
