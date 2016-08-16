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
# Copyright (c) 2008-2016 Cisco Systems, Inc.  All rights reserved.
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
AC_DEFUN([MCA_opal_memory_patcher_PRIORITY], [41])

AC_DEFUN([MCA_opal_memory_patcher_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_memory_patcher_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_memory_patcher_CONFIG],[
    AC_CONFIG_FILES([opal/mca/memory/patcher/Makefile])

    AC_CHECK_FUNCS([__curbrk])

    AC_CHECK_HEADERS([linux/mman.h sys/syscall.h])

    AC_CHECK_DECLS([__mmap], [], [], [#include <sys/mman.h>])

    AC_CHECK_FUNCS([__mmap])

    AC_CHECK_DECLS([__syscall], [], [], [#include <sys/syscall.h>])

    AC_CHECK_FUNCS([__syscall])

    [$1]
])
