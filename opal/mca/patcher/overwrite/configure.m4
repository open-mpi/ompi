# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_patcher_overwrite_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_patcher_overwrite_CONFIG],[
    AC_CONFIG_FILES([opal/mca/patcher/overwrite/Makefile])

    opal_patcher_overwrite_happy=no
    if test $OPAL_ENABLE_DLOPEN_SUPPORT = 1; then
# Disable ia64 for now. We can revive it later if anyone cares
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if !defined(__i386__) && !defined(__x86_64__) && !defined(__PPC__) && !defined(__aarch64__)
#error "platform not supported"
#endif
]],[])],[opal_patcher_overwrite_happy=yes],[])
    fi

    AS_IF([test $opal_patcher_overwrite_happy = yes], [$1], [$2])
])
