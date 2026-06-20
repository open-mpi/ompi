# -*- shell-script -*-
#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2014-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OMPI_FIND_MPI_AINT_COUNT_OFFSET],[
    _OMPI_FIND_MPI_AINT_TYPE
    _OMPI_FIND_MPI_COUNT_TYPE
    _OMPI_FIND_MPI_OFFSET_TYPE

    if test "$ompi_fortran_happy" = "1" && \
       test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS"; then
        _OMPI_FIND_MPI_INTEGER_KIND
        _OMPI_FIND_MPI_ADDRESS_KIND
        _OMPI_FIND_MPI_COUNT_KIND
        _OMPI_FIND_MPI_OFFSET_KIND
    fi
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_AINT_TYPE], [
    # MPI_Aint type is ptrdiff_t; just use that.
    AC_MSG_CHECKING([for type of MPI_Aint])

    MPI_AINT_TYPE=ptrdiff_t
    # Get the size of this type; we'll need it to figure out Fortran's
    # MPI_ADDRESS_KIND, later
    MPI_AINT_SIZE=$ac_cv_sizeof_ptrdiff_t
    AC_DEFINE_UNQUOTED(OMPI_MPI_AINT_TYPE, [$MPI_AINT_TYPE],
                       [Type of MPI_Aint])

    AC_MSG_RESULT([$MPI_AINT_TYPE (size: $MPI_AINT_SIZE)])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_COUNT_TYPE], [
    # MPI_Count shares its backing type with opal_count_t.  Rather than
    # recompute it here, derive it from the OPAL_COUNT_* shell variables that
    # OPAL_FIND_COUNT_TYPE already computed (the single source of truth, run
    # unconditionally in configure.ac before this macro).  This guarantees
    # sizeof(MPI_Count) == sizeof(opal_count_t); a build-time _Static_assert in
    # OMPI enforces it.  See config/opal_find_count_type.m4 for the rationale
    # behind the type selection (<= size_t, >= ptrdiff_t, not the widest type).

    AC_MSG_CHECKING([for type of MPI_Count])

    MPI_COUNT_TYPE=$OPAL_COUNT_TYPE
    MPI_COUNT_SIZE=$OPAL_COUNT_SIZE
    MPI_COUNT_MAX=$OPAL_COUNT_MAX

    # The corresponding MPI datatype (also reused for MPI_Offset, below).
    case "$MPI_COUNT_TYPE" in
        "long long") MPI_COUNT_DATATYPE=MPI_LONG_LONG ;;
        long)        MPI_COUNT_DATATYPE=MPI_LONG ;;
        int)         MPI_COUNT_DATATYPE=MPI_INT ;;
        *)
            AC_MSG_RESULT([$MPI_COUNT_TYPE])
            AC_MSG_ERROR([*** No corresponding MPI datatype for MPI_Count type "$MPI_COUNT_TYPE"])
            ;;
    esac

    AC_DEFINE_UNQUOTED(OMPI_MPI_COUNT_SIZE, $MPI_COUNT_SIZE,
                       [Size of the MPI_Count datatype])
    AC_DEFINE_UNQUOTED(OMPI_MPI_COUNT_TYPE, $MPI_COUNT_TYPE,
                       [Type of the MPI_Count datatype])
    AC_DEFINE_UNQUOTED(MPI_COUNT_MAX, $MPI_COUNT_MAX,
                       [Maximum value for an MPI_Count])

    AC_MSG_RESULT([$MPI_COUNT_TYPE (size: $MPI_COUNT_SIZE)])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_OFFSET_TYPE], [
    # Just make MPI_Offset be the same as MPI_Count
    AC_MSG_CHECKING([for type of MPI_Offset])

    MPI_OFFSET_TYPE=$MPI_COUNT_TYPE
    AC_DEFINE_UNQUOTED(OMPI_MPI_OFFSET_TYPE, $MPI_OFFSET_TYPE,
                       [Type of MPI_Offset])
    MPI_OFFSET_SIZE=$MPI_COUNT_SIZE
    AC_DEFINE_UNQUOTED(OMPI_MPI_OFFSET_SIZE, $MPI_OFFSET_SIZE,
                       [Size of the MPI_Offset])
    AC_MSG_RESULT([$MPI_COUNT_TYPE (size: $MPI_OFFSET_SIZE)])

    AC_MSG_CHECKING([for an MPI datatype for MPI_Offset])
    MPI_OFFSET_DATATYPE=$MPI_COUNT_DATATYPE
    AC_DEFINE_UNQUOTED(OMPI_OFFSET_DATATYPE, $MPI_OFFSET_DATATYPE,
                       [MPI datatype corresponding to MPI_Offset])
    AC_MSG_RESULT([$MPI_OFFSET_DATATYPE])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_INTEGER_KIND], [
    # Get the kind value for Fortran MPI_INTEGER_KIND (corresponding
    # to whatever is the same size as a F77 INTEGER -- for the
    # most-likely-will-never-occur case where F77 INTEGER is smaller
    # than an F90 INTEGER; see MPI-2 4.12.6.5.  As with OMPI
    # FORTRAN_CHECK, use the official BIND(C) KIND names (see comment
    # in fortran_check.m4).
    AC_MSG_CHECKING([for MPI_INTEGER_KIND])
    if test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 2; then
        OMPI_MPI_INTEGER_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 4; then
        OMPI_MPI_INTEGER_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 8; then
        OMPI_MPI_INTEGER_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $OMPI_SIZEOF_FORTRAN_INTEGER -eq 16; then
        AC_MSG_ERROR([Cannot support Fortran MPI_INTEGER_KIND!])
    fi
    AC_SUBST(OMPI_MPI_INTEGER_KIND)
    AC_MSG_RESULT([$OMPI_MPI_INTEGER_KIND])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_ADDRESS_KIND], [
    # Get the kind value for Fortran MPI_ADDRESS_KIND (corresponding
    # to whatever is big enough to hold MPI_Aint).
    AC_MSG_CHECKING([for MPI_ADDRESS_KIND])
    if test $MPI_AINT_SIZE -eq 2 ; then
        OMPI_MPI_ADDRESS_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $MPI_AINT_SIZE -eq 4 ; then
        OMPI_MPI_ADDRESS_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $MPI_AINT_SIZE -eq 8 ; then
        OMPI_MPI_ADDRESS_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $MPI_AINT_SIZE -eq 16 ; then
        AC_MSG_ERROR([Cannot support Fortran MPI_ADDRESS_KIND!])
    fi
    AC_SUBST(OMPI_MPI_ADDRESS_KIND)
    AC_MSG_RESULT([$OMPI_MPI_ADDRESS_KIND])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_COUNT_KIND], [
    # Get the kind value for Fortran MPI_COUNT_KIND (corresponding to
    # whatever is big enough to hold an MPI_Count)
    AC_MSG_CHECKING([for MPI_COUNT_KIND])
    if test $MPI_COUNT_SIZE -eq 2 ; then
        OMPI_MPI_COUNT_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $MPI_COUNT_SIZE -eq 4 ; then
        OMPI_MPI_COUNT_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $MPI_COUNT_SIZE -eq 8 ; then
        OMPI_MPI_COUNT_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $MPI_COUNT_SIZE -eq 16 ; then
        AC_MSG_ERROR([Cannot support Fortran MPI_COUNT_KIND!])
    fi
    AC_SUBST(OMPI_MPI_COUNT_KIND)
    AC_MSG_RESULT([$OMPI_MPI_COUNT_KIND])
])

dnl #########################################################################

AC_DEFUN([_OMPI_FIND_MPI_OFFSET_KIND], [
    # Get the kind value for Fortran MPI_OFFSET_KIND (corresponding to
    # whatever is big enough to hold an MPI_Offset)
    AC_MSG_CHECKING([for MPI_OFFSET_KIND])
    if test $MPI_OFFSET_SIZE -eq 2 ; then
        OMPI_MPI_OFFSET_KIND=$OMPI_FORTRAN_C_INT16_T_KIND
    elif test $MPI_OFFSET_SIZE -eq 4 ; then
        OMPI_MPI_OFFSET_KIND=$OMPI_FORTRAN_C_INT32_T_KIND
    elif test $MPI_OFFSET_SIZE -eq 8 ; then
        OMPI_MPI_OFFSET_KIND=$OMPI_FORTRAN_C_INT64_T_KIND
    elif test $MPI_OFFSET_SIZE -eq 16 ; then
        AC_MSG_ERROR([Cannot support Fortran MPI_OFFSET_KIND!])
    fi
    AC_SUBST(OMPI_MPI_OFFSET_KIND)
    AC_MSG_RESULT([$OMPI_MPI_OFFSET_KIND])
])
