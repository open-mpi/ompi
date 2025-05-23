dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2025      Stony Brook University. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Check whether the fortran compiler produces a warning when
# it encounters a #warning directive

# OMPI_FORTRAN_CHECK_WARNING([action if found],
#                            [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_WARNING],[
    AS_VAR_PUSHDEF([warning_var], [ompi_cv_fortran_warning])

    AC_CACHE_CHECK([if Fortran compiler supports preprocessor warnings], warning_var,
        [
        # check if the compiler provides a proper #warning
        # some compilers (gfortran) do not show the warning if the file is found
        # through an include path, so create a temporary directory and include file
        OAC_VAR_SCOPE_PUSH(msg, dir)
        dir="tmp_includedir_$$"
        msg="This is a deprecated file"
        AS_MKDIR_P($dir)
        echo "#warning $msg" > $dir/deprecated.h

        echo "! -*- fortran -*-
            program main
                implicit none
                include 'deprecated.h'
            end program main
        " > conftest.f
        AS_IF([${FC} ${FCFLAGS} -c -I$dir conftest.f 2>conftest.err >conftest.out],
              [ # compilation succeeded, check the produced output for the warning
                AS_IF([grep "$msg" conftest.err conftest.out >/dev/null 2>/dev/null],
                      [AS_VAR_SET(warning_var, "yes")],
                      [AS_VAR_SET(warning_var, "no (missing warning)")],)],
              [AS_VAR_SET(warning_var, "no (compilation failed)")])
        OPAL_VAR_SCOPE_POP
        rm -rf conftest.f conftest.err conftest.out $dir 2>/dev/null >/dev/null
    ])
    AS_VAR_IF(warning_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([warning_var])dnl
])

