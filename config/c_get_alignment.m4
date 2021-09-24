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
dnl Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL_C_GET_ALIGNMENT(c_type, c_macro_name)
# ----------------------------------
# Determine datatype alignment.
# First arg is type, 2nd arg is macro name to define.
# Now that we require C99 compilers, we include stdbool.h
# in the alignment test so that we can find the definition
# of "bool" when we test for its alignment. We might be able
# to avoid this if we test for alignemtn of _Bool, but
# since we use "bool" in the code, let's be safe and check
# what we use. Yes, they should be the same - but "should" and
# "are" frequently differ
AC_DEFUN([OPAL_C_GET_ALIGNMENT],[
    OPAL_VAR_SCOPE_PUSH([opal_align_value])
    AC_LANG_PUSH([C])

    AC_CHECK_ALIGNOF([$1], [AC_INCLUDES_DEFAULT
                            #include <stdbool.h>
                           ])

    # Put the value determined from AC CHECK_ALIGNOF into an
    # easy-to-access shell variable.
    AS_VAR_COPY([opal_align_value],
                [ac_cv_alignof_]AS_TR_SH([$1]))

    # This $opal_cv_c_align_* shell variable is used elsewhere in
    # configure.ac
    AS_VAR_COPY([opal_cv_c_align_]AS_TR_SH([$1]),
                [opal_align_value])

    # This #define is used in C code.
    AC_DEFINE_UNQUOTED([$2], [$opal_align_value], [Alignment of $1])

    AC_LANG_POP([C])
    OPAL_VAR_SCOPE_POP
])
