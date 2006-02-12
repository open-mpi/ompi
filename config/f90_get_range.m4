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

# OMPI_F90_GET_RANGE(type, shell variable to set)
# -----------------------------------------------
AC_DEFUN([OMPI_F90_GET_RANGE],[
    AS_VAR_PUSHDEF([type_var], [ompi_cv_f90_range_$1])

    AC_CACHE_CHECK([range of Fortran 90 $1], type_var,
       [cat > conftestf.f90 <<EOF
program f90range
    $1 :: x
    open(8, file="conftestval")
    write(8, fmt="(I5)") range(x)
    close(8)
end program
EOF

        # Compile
        OMPI_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 -o conftest conftestf.f90 $LDFLAGS $LIBS], [happy="yes"], [happy="no"])

        if test "$happy" = "no"; then
             OMPI_LOG_MSG([here is the fortran 90 program:], 1)
             OMPI_LOG_FILE([conftestf.f90])
             AC_MSG_WARN([Could not determine range of $1])
             AC_MSG_WARN([See config.log for details])
             AC_MSG_ERROR([Cannot continue])
        fi

        # If not cross compiling, try to run (if we're cross
        # compiling, then the value should have been loaded by the
        # cache already)
        AS_IF([test "$cross_compiling" = "yes"],
            [AC_MSG_ERROR([Can not determine range of $1 when cross-compiling])],
            [OMPI_LOG_COMMAND([./conftest],
                [AS_VAR_SET(type_var, [`sed 's/  *//' conftestval`])],
                [AC_MSG_ERROR([Could not determine range of $1])])])

        unset happy
        /bin/rm -f conftest*])

    $2=AS_VAR_GET([type_var])
    AS_VAR_POPDEF([type_var])dnl
])
