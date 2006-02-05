dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_CHECK_COMPILER_WORKS(language, language_exit,
#            [action-if-found], [action-if-not-found])
# ----------------------------------------------------
# Try to compile and run a simple "exit(0)" application in
# 'language'.  A warning is always printed if the application
# fails to run.  Action-if-found is evaluated if the application
# runs successfully (or compiles if cross-compiling), and
# action-if-not-found is evaluated if the application fails to
# run.
#
# language-exit should be how to exit cleanly in 'language'.
# You probably want exit(0) for C/C++ and empty for Fortran.
AC_DEFUN([OMPI_CHECK_COMPILER_WORKS],
[   AS_VAR_PUSHDEF([lang_var], [ompi_cv_$1_works])

    AC_CACHE_CHECK([if $1 compiler works], lang_var,
        [AC_LANG_PUSH($1)
         AC_RUN_IFELSE([AC_LANG_PROGRAM([], [$2])],
                       [AS_VAR_SET(lang_var, ["yes"])],
                       [AS_VAR_SET(lang_var, ["no"])],
                       [AS_VAR_SET(lang_var, ["cross compiling"])])
         AC_LANG_POP($1)])
    AS_IF([test "AS_VAR_GET(lang_var)" = "no"],
          [cat <<EOF >&2
**********************************************************************
* It appears that your $1 compiler is unable to produce working
* executables.  A simple test application failed to properly
* execute.  Note that this is likely not a problem with Open MPI,
* but a problem with the local compiler installation.  More
* information (including exactly what command was given to the 
* compiler and what error resulted when the command was executed) is
* available in the config.log file in this directory.
**********************************************************************
EOF
           $4], [$3])

    AS_VAR_POPDEF([lang_var])dnl
])
