dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl


# OMPI_FORTRAN_CHECK(Fortran type, c type required, types to search, 
#                    expected size, define ompi_fortran_<foo>_t or not))
#----------------------------------------------------------
# Check Fortran type, including:
# - whether compiler supports or not
# - size of type
# - equal to expected size
# - alignment
# - associated C type
#
# types to search is a comma-seperated list of values
AC_DEFUN([OMPI_FORTRAN_CHECK], [
    OPAL_VAR_SCOPE_PUSH([ofc_have_type ofc_type_size ofc_type_alignment ofc_c_type ofc_expected_size])

    ofc_expected_size=$4
    ofc_define_type=$5
    ofc_have_type=0
    ofc_type_size=$ac_cv_sizeof_int
    ofc_type_alignment=$ac_cv_sizeof_int
    ofc_c_type=ompi_fortran_bogus_type_t
    ofc_type_kind=0

    # Only check if we actually want the Fortran bindings / have a
    # Fortran compiler.  This allows us to call this macro, even if
    # there is no Fortran compiler.  If we have no Fortran compiler,
    # then just set a bunch of defaults.
    if test $OMPI_WANT_FORTRAN_BINDINGS -eq 1; then
        OMPI_FORTRAN_CHECK_TYPE([$1], [ofc_have_type=1], [ofc_have_type=0])
    else
        AC_MSG_CHECKING([if Fortran compiler supports $1])
        AC_MSG_RESULT([skipped])
    fi

    if test "$ofc_have_type" = "1"; then
        # What is the size of this type?  

        # NOTE: Some Fortran compilers actually will return that a
        # type exists even if it doesn't support it -- the compiler
        # will automatically convert the unsupported type to a type
        # that it *does* support.  For example, if you try to use
        # INTEGER*16 and the compiler doesn't support it, it may well
        # automatically convert it to INTEGER*8 for you (!).  So we
        # have to check the actual size of the type once we determine
        # that the compiler doesn't error if we try to use it
        # (i.e,. the compiler *might* support that type).  If the size
        # doesn't match the expected size, then the compiler doesn't
        # really support it.
        OMPI_FORTRAN_GET_SIZEOF([], [$1], [ofc_type_size])
        if test "$ofc_expected_size" != "-1" -a "$ofc_type_size" != "$ofc_expected_size"; then
            AC_MSG_WARN([*** Fortran $1 does not have expected size!])
            AC_MSG_WARN([*** Expected $ofc_expected_size, got $ofc_type_size])
            AC_MSG_WARN([*** Disabling MPI support for Fortran $1])
            ofc_have_type=0
        else
            # Look for a corresponding C type (will abort by itself if the
            # type isn't found and we need it)
            ofc_c_type=
            m4_ifval([$3], [OMPI_FIND_TYPE([$1], [$3], [$2], [$ofc_type_size], [ofc_c_type])
            if test -z "$ofc_c_type" ; then
                ofc_have_type=0
            fi])

            # Get the alignment of the type
            if test "$ofc_have_type" = "1"; then
                OMPI_FORTRAN_GET_ALIGNMENT([$1], [ofc_type_alignment])

                # Add it to the relevant list of types found
                if test "$ofc_expected_size" != "-1"; then
                    ofc_letter=m4_translit(m4_bpatsubst($1, [^\(.\).+], [[\1]]), [a-z], [A-Z])
                    ofc_str="OMPI_FORTRAN_${ofc_letter}KINDS=\"\$OMPI_FORTRAN_${ofc_letter}KINDS $ofc_type_size \""
                    eval $ofc_str
                fi
            fi

            # Get the kind of the type.  We do this by looking at the
            # Fortran type's corresponding C type (which we figured
            # out above).  Then we look a the official BIND(C) KIND
            # type for that type.  The official BIND(C) types were
            # taken from table 15.2 of the Fortran 2008 standard,
            # published on 6 October as ISO/IEC 1539-1:2010 (this is
            # not a free document). A copy of this table is in the
            # file ompi/mpi/fortran/c_to_integer_kind_mapping.pdf.

            # NOTE: Some of the values of these C_* constants *may be
            # negative* if the compiler doesn't support them.  We have
            # already verified that both the Fortran and the C types
            # both exist.  However, the compiler may still have -1 for
            # the C_<foo> constants if the C type is not the same
            # format as its corresponding Fortran type (e.g., Absoft's
            # "REAL*16" and "long double" are the same size, but not
            # the same format -- so the compiler is allowed to define
            # C_LONG_DOUBLE to -1).

            AC_MSG_CHECKING([for corresponding KIND value of $1])
            case "$ofc_c_type" in
            char)     	          ofc_type_kind=C_SIGNED_CHAR         ;;
            double)               ofc_type_kind=C_DOUBLE              ;;
            float)                ofc_type_kind=C_FLOAT               ;;
            int)                  ofc_type_kind=C_INT                 ;;
            int16_t)              ofc_type_kind=C_INT16_T             ;;
            int32_t)              ofc_type_kind=C_INT32_T             ;;
            int64_t)              ofc_type_kind=C_INT64_T             ;;
            int8_t)               ofc_type_kind=C_INT8_T              ;;
            long)                 ofc_type_kind=C_LONG                ;;
            long*double)          ofc_type_kind=C_LONG_DOUBLE         ;;
            long*long)            ofc_type_kind=C_LONG_LONG           ;;
            short)                ofc_type_kind=C_SHORT               ;;
            float*_Complex)       ofc_type_kind=C_FLOAT_COMPLEX       ;;
            double*_Complex)      ofc_type_kind=C_DOUBLE_COMPLEX      ;;
            long*double*_Complex) ofc_type_kind=C_LONG_DOUBLE_COMPLEX ;;
            *)
                # Skip types like "DOUBLE PRECISION"
                ;;
            esac
            AS_IF([test "$ofc_type_kind" != ""],
                  [AC_MSG_RESULT([$ofc_type_kind])],
                  [AC_MSG_RESULT([<skipped>])])

            # See if the value is -1.  If so, then just say we don't
            # have a match.  If the compiler doesn't support
            # ISO_C_BINDING, then we'll just fall back to a default
            # kind and hope for the best.  :-\
            OMPI_FORTRAN_GET_KIND_VALUE([$ofc_type_kind], 4, [ofc_type_kind_value])
            AS_IF([test $ofc_type_kind_value -le 0],
                  [ofc_have_type=0
                   AC_MSG_WARN([Compiler $1 and $ofc_c_type mismatch; MPI datatype unsupported])])
        fi
    fi

    # We always need these defines -- even if we don't have a given
    # type, there are some places in the code where we have to have
    # *something*.  Note that the bpatsubst's are the same as used
    # above (see comment above), but we added a translit to make them
    # uppercase.

    # If we got a pretty name, use that as the basis.  If not, use the
    # first part of the provided fortran type (e.g.,
    # "logical(selected_int_kind(2))" -> logical1")

    # Finally, note that it is necessary to use the Big Long Ugly m4
    # expressions in the AC_DEFINE_UNQUOTEDs.  If you don't (e.g., put
    # the result of the BLUm4E in a shell variable and use that in
    # AC_DEFINE_UNQUOTED), autoheader won't put them in the
    # AC_CONFIG_HEADER (or AM_CONFIG_HEADER, in our case).
    AC_DEFINE_UNQUOTED([OMPI_HAVE_FORTRAN_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]),
                       [$ofc_have_type], 
                       [Whether we have Fortran $1 or not])
    AC_DEFINE_UNQUOTED([OMPI_SIZEOF_FORTRAN_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]),
                       [$ofc_type_size], 
                       [Size of Fortran $1])
    AC_DEFINE_UNQUOTED([OMPI_ALIGNMENT_FORTRAN_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]),
                       [$ofc_type_alignment], 
                       [Alignment of Fortran $1])
    AC_DEFINE_UNQUOTED([OMPI_KIND_FORTRAN_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]),
                       [$ofc_type_kind], 
                       [Fortrn KIND number for $1])
    if test "$3" != "" -a "$ofc_define_type" = "yes"; then
        AC_DEFINE_UNQUOTED([ompi_fortran_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [A-Z], [a-z])[_t],
                           [$ofc_c_type], 
                           [C type corresponding to Fortran $1])
    fi

    # Save some in shell variables for later use (e.g., need
    # OMPI_SIZEOF_FORTRAN_INTEGER in OMPI_FORTRAN_GET_HANDLE_MAX)
    [OMPI_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[_C_TYPE=$ofc_c_type]
    [OMPI_KIND_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[=$ofc_type_kind]
    [OMPI_HAVE_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[=$ofc_have_type]
    [OMPI_SIZEOF_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[=$ofc_type_size]
    [OMPI_ALIGNMENT_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_])[=$ofc_type_alignment]

    # Wow, this is sick.  But it works!  :-)
    AC_SUBST([OMPI_HAVE_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]))
    AC_SUBST([OMPI_KIND_FORTRAN_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [a-z], [A-Z]))
    AC_SUBST([OMPI_SIZEOF_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]))
    AC_SUBST([OMPI_SIZEOF_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]))
    AC_SUBST([OMPI_ALIGNMENT_FORTRAN_]m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]))

    # Clean up
    OPAL_VAR_SCOPE_POP
])dnl
