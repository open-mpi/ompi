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

define(OMPI_F77_FIND_EXT_SYMBOL_CONVENTION,[
AC_MSG_CHECKING($1 external symbol convention)

ompi_fortran_double_underscore=0
ompi_fortran_single_underscore=0
ompi_fortran_caps=0
ompi_fortran_plain=0

# If we didn't find an f77 compiler, or if we don't want the f77
# bindings, just leave everything hardwired to 0.  Otherwise, do the
# real test.

if test "$1" = "none" -o "$OMPI_WANT_F77_BINDINGS" = "0"; then
    AC_MSG_RESULT([no Fortran 77 bindings -- skipped])
else
    cat > conftestf.f <<EOF
       subroutine FOO_bar(a)
       integer a
       a = 1
       return
       end
EOF
    $1 $FFLAGS -c conftestf.f 1>&5 2>&1
    if test ! -s conftestf.o; then
        AC_MSG_WARN(unable to produce an object file testing F77 compiler)
    else
        nm conftestf.o | grep foo_bar__ > /dev/null 2>&1
        if test $? = "0"; then 
            AC_MSG_RESULT([double underscore])
            ompi_fortran_double_underscore=1
            ompi_ac_doubleunder=y
        else
            nm conftestf.o | grep foo_bar_ > /dev/null 2>&1
            if test $? = "0"; then 
                AC_MSG_RESULT([single underscore])
                ompi_fortran_single_underscore=1
                ompi_ac_singleunder=y
            else
                # We may get into trouble here if we start accepting
                # mixed case compilers -- we may need to have caps
                # underscore, or caps double underscore, for example.
                # But we haven't found any that require that yet.  :-)
                nm conftestf.o | grep FOO_bar > /dev/null 2>&1
                if test $? = "0"; then 
                    AC_MSG_RESULT([mixed case, so FORTRANCAPS])
                    ompi_fortran_caps=1
                    ompi_ac_caps=y
                else
                    nm conftestf.o | grep foo_bar > /dev/null 2>&1
                    if test $? = "0"; then 
                        AC_MSG_RESULT([no underscore])
                        ompi_fortran_plain=1
                        ompi_ac_nounder=y
                    else
                        nm conftestf.o | grep FOO_BAR > /dev/null 2>&1
                        if test $? = "0"; then 
                            AC_MSG_RESULT([all upper case])
                            ompi_fortran_caps=1
                            ompi_ac_caps=y
                        else
                            AC_MSG_WARN([*** Could not find name of subroutine foo_bar])
                            AC_MSG_ERROR([Cannot continue])
                        fi
                    fi
                fi
            fi
        fi
    fi
fi

AC_DEFINE_UNQUOTED(OMPI_F77_DOUBLE_UNDERSCORE,
    $ompi_fortran_double_underscore, 
    [Whether fortran symbols have a trailing double underscore or not])
AC_DEFINE_UNQUOTED(OMPI_F77_SINGLE_UNDERSCORE, $ompi_fortran_single_underscore,
    [Whether fortran symbols have a trailing underscore or not])
AC_DEFINE_UNQUOTED(OMPI_F77_CAPS, $ompi_fortran_caps,
    [Whether fortran symbols are all caps or not])
AC_DEFINE_UNQUOTED(OMPI_F77_PLAIN, $ompi_fortran_plain,
    [Whether fortran symbols have no trailing underscore or not])

/bin/rm -f conftestf.f conftestf.o])dnl
