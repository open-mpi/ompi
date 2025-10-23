# -*- shell-script -*-
#
# Copyright (C) 2015-2017 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# Copyright (c) 2024-2025 Bull S.A.S.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_UBCL(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if UBCL support can be found.  sets prefix_{CPPFLAGS,
# as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_UBCL],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_ubcl_dir ompi_check_ubcl_happy])

    m4_ifblank([$1], [m4_fatal([First argument to OMPI_CHECK_UBCL cannot be blank])])

    AC_ARG_WITH([ubcl],
        [AC_HELP_STRING([--with-ubcl(=DIR)],
            [Build with UBCL support])])

    # UBCL is dlopen'd to avoid direct link to libubcl.so.
    # OAC_CHECK_PACKAGE would add this explicit link, so it cannot be used.
    # OPAL_CHECK_WITHDIR prints an error if the given path is invalid
    OPAL_CHECK_WITHDIR([ubcl], [$with_ubcl], [include/ubcl_api.h])

    AS_IF([test "$with_ubcl" == "no"],
          [ompi_check_ubcl_happy="no"],

          [test -z "$with_ubcl"],
          [ompi_check_ubcl_happy="no"],

          [ompi_check_ubcl_happy="yes"
           $1_CPPFLAGS="${$1_CPPFLAGS} -I$with_ubcl/include/"
           AC_MSG_NOTICE([$1_CPPFLAGS is set to: ${$1_CPPFLAGS}])])


    OPAL_SUMMARY_ADD([Transports],[UBCL],[],[$ompi_check_ubcl_happy])

    AS_IF([test "$ompi_check_ubcl_happy" = "yes"],
          [$2],
          [$3])

    OPAL_VAR_SCOPE_POP
])

