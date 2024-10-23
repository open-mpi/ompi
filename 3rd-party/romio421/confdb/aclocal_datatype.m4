dnl Utilities in shell functions - reduces size in configure script
dnl These functions sets $pac_retval
dnl
AC_DEFUN([PAC_DATATYPE_UTILS], [
# Take decimal and turn it into two hex digits
to_hex() {
    len=$[]1
    if test -z $len ; then
        pac_retval="00"
    elif test $len -le 9 ; then
        dnl avoid subshell for speed
        pac_retval="0${len}"
    elif test $len -eq 16 ; then
        pac_retval="10"
    elif test $len -eq 32 ; then
        pac_retval="20"
    else
        dnl printf is portable enough
        pac_retval=`printf "%02x" $len`
    fi
}

# Convert hex values in the form of 0x######## to decimal for
# datatype constants in Fortran.
to_dec() {
    orig_value=$[]1
    dnl printf is portable enough
    pac_retval=`printf "%d" $orig_value`
}

# return the C type matching integer of size
get_c_int_type() {
    len=$[]1
    pac_retval=
    for c_type in char short int long "long_long" ; do
        eval ctypelen=\$"ac_cv_sizeof_$c_type"
        if test "$len" = "$ctypelen" -a "$ctypelen" -gt 0 ; then
            if test "$c_type" = "long_long" ; then
                pac_retval=`echo $c_type | sed -e 's/_/ /g'`
            else
                pac_retval=$c_type
            fi
            return
        fi
    done
    pac_retval="unavailable"
}

# return the C type matching integer of size
get_c_float_type() {
    len=$[]1
    pac_retval=
    for c_type in float double __float128 "long_double" ; do
        eval ctypelen=\$"ac_cv_sizeof_$c_type"
        if test "$len" = "$ctypelen" -a "$ctypelen" -gt 0 ; then
            if test "$c_type" = "long_double" ; then
                pac_retval=`echo $c_type | sed -e 's/_/ /g'`
            else
                pac_retval=$c_type
            fi
            return
        fi
    done
    pac_retval="unavailable"
}

# return (via $pac_retval) the C type matching integer of size
get_c_bool_type() {
    len=$[]1
    pac_retval=
    for c_type in _Bool unsigned_char unsigned_short unsigned_int unsigned_long unsigned_long_long ; do
        eval ctypelen=\$"ac_cv_sizeof_$c_type"
        if test "$len" = "$ctypelen" -a "$ctypelen" -gt 0 ; then
            if test "$c_type" != "_Bool" ; then
                pac_retval=`echo $c_type | sed -e 's/_/ /g'`
            else
                pac_retval=$c_type
            fi
            return
        fi
    done
    pac_retval="unavailable"
}
])

dnl
dnl PAC_SET_MPI_TYPE(hexidx, name, len, [multiplier])
dnl Defines datatype hex handle
dnl     e.g. PAC_SET_MPI_TYPE(01, MPI_CHAR, 1) -> MPI_CHAR=0x4c000101
dnl Multiplier can be used to define double types
dnl     e.g. PAC_SET_MPI_TYPE(48, MPI_COMPLEX, $pac_cv_f77_sizeof_real, 2)
dnl
AC_DEFUN([PAC_SET_MPI_TYPE], [
    if test -z "$3" -o "$3" = 0 ; then
        $2=MPI_DATATYPE_NULL
        F77_$2=MPI_DATATYPE_NULL
        F08_$2=MPI_DATATYPE_NULL%MPI_VAL
    else
        hexidx=$1

        if test -n "$4" ; then
            len=`expr $3 \* $4`
	else
            len=$3
        fi
        to_hex $len

        hexlen=$pac_retval

        $2="0x4c00${hexlen}${hexidx}"
        to_dec $[]$2
        F77_$2=$pac_retval
        F08_$2=$pac_retval
    fi
    AC_SUBST($2)
    AC_SUBST(F77_$2)
    AC_SUBST(F08_$2)
])

dnl
dnl PAC_SET_MPI_LBUB(hex_idx, name)
dnl     e.g. PAC_SET_MPI_LBUB(10, MPI_LB) -> MPI_LB=0x4c000010
dnl
AC_DEFUN([PAC_SET_MPI_LBUB], [
    $2="0x4c0000$1"
    to_dec $[]$2
    F77_$2=$pac_retval
    F08_$2=$pac_retval
    AC_SUBST($2)
    AC_SUBST(F77_$2)
    AC_SUBST(F08_$2)
])

dnl
dnl PAC_SET_MPI_PAIRTYPE(hexidx, name)
dnl     e.g. PAC_SET_MPI_PAIRTYPE(02, MPI_LONG_INT) -> MPI_LONG_INT=0x8c000002
dnl
AC_DEFUN([PAC_SET_MPI_PAIRTYPE], [
    $2="0x8c0000$1"
    # 0x8c000000 is -1946157056
    pac_retval=`expr $1 - 1946157056`
    F77_$2=$pac_retval
    F08_$2=$pac_retval
    AC_SUBST($2)
    AC_SUBST(F77_$2)
    AC_SUBST(F08_$2)
])

dnl
dnl PAC_SET_MPI_DATATYPE_ALIAS(name, alias)
dnl
AC_DEFUN([PAC_SET_MPI_DATATYPE_ALIAS], [
    $1=$[]$2
    F77_$1=$F77_$2
    F08_$1=$F08_$2
    AC_SUBST($1)
    AC_SUBST(F77_$1)
    AC_SUBST(F08_$1)
])

dnl
dnl PAC_F77_CHECK_FIXED_REAL(size) and PAC_F77_CHECK_FIXED_INTEGER(size)
dnl Map fixed-size Fortran types, e.g. REAL*4, to equivallent C types.
dnl If no equivallent C types exist, set corresponding sizeof value to 0.
dnl
AC_DEFUN([PAC_F77_CHECK_FIXED_REAL], [
    get_c_float_type $1
    if test "$pac_retval" = "unavailable" ; then
        eval pac_cv_f77_sizeof_real$1=0
    else
        eval pac_cv_f77_sizeof_real$1=$1
        AC_DEFINE_UNQUOTED(MPIR_REAL$1_CTYPE,$pac_retval,[C type to use for MPI_REAL$1])
    fi
])

AC_DEFUN([PAC_F77_CHECK_FIXED_INTEGER], [
    get_c_int_type $1
    if test "$pac_retval" = "unavailable" ; then
        eval pac_cv_f77_sizeof_integer$1=0
    else
        eval pac_cv_f77_sizeof_integer$1=$1
        AC_DEFINE_UNQUOTED(MPIR_INTEGER$1_CTYPE,$pac_retval,[C type to use for MPI_INTEGER$1])
    fi
])

