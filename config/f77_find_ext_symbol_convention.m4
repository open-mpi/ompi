dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

define(LAM_F77_FIND_EXT_SYMBOL_CONVENTION,[
AC_MSG_CHECKING($1 external symbol convention)

lam_fortran_double_underscore=0
lam_fortran_single_underscore=0
lam_fortran_caps=0
lam_fortran_plain=0

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
	lam_fortran_double_underscore=1
	lam_ac_doubleunder=y
    else
	nm conftestf.o | grep foo_bar_ > /dev/null 2>&1
	if test $? = "0"; then 
	    AC_MSG_RESULT([single underscore])
	    lam_fortran_single_underscore=1
	    lam_ac_singleunder=y
	else
	    # We may get into trouble here if we start accepting mixed
	    # case compilers -- we may need to have caps underscore,
	    # or caps double underscore, for example.  But we haven't
	    # found any that require that yet.  :-)
	    nm conftestf.o | grep FOO_bar > /dev/null 2>&1
	    if test $? = "0"; then 
		AC_MSG_RESULT([mixed case, so FORTRANCAPS])
		lam_fortran_caps=1
		lam_ac_caps=y
	    else
		nm conftestf.o | grep foo_bar > /dev/null 2>&1
		if test $? = "0"; then 
		    AC_MSG_RESULT([no underscore])
		    lam_fortran_plain=1
		    lam_ac_nounder=y
		else
		    nm conftestf.o | grep FOO_BAR > /dev/null 2>&1
		    if test $? = "0"; then 
			AC_MSG_RESULT([all upper case])
			lam_fortran_caps=1
			lam_ac_caps=y
		    else
			AC_MSG_WARN([*** Could not find name of subroutine foo_bar])
			AC_MSG_ERROR([Cannot continue])
		    fi
		fi
	    fi
	fi
    fi
fi

AC_DEFINE_UNQUOTED(LAM_F77_DOUBLE_UNDERSCORE,
    $lam_fortran_double_underscore, 
    [Whether fortran symbols have a trailing double underscore or not])
AC_DEFINE_UNQUOTED(LAM_F77_SINGLE_UNDERSCORE, $lam_fortran_single_underscore,
    [Whether fortran symbols have a trailing underscore or not])
AC_DEFINE_UNQUOTED(LAM_F77_CAPS, $lam_fortran_caps,
    [Whether fortran symbols are all caps or not])
AC_DEFINE_UNQUOTED(LAM_F77_PLAIN, $lam_fortran_plain,
    [Whether fortran symbols have no trailing underscore or not])

/bin/rm -f conftestf.f conftestf.o])dnl
