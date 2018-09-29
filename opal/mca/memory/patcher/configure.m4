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
    # disable on MacOS/Darwin where it isn't used and the deprecated
    # syscall interface causes compiler warnings.
    AC_MSG_CHECKING([if memory patcher supports $host_os])
    case $host_os in
    darwin*)
        opal_memory_patcher_happy=no
    ;;
    *)
        opal_memory_patcher_happy=yes
    ;;
    esac
    AC_MSG_RESULT([$opal_memory_patcher_happy])

    AS_IF([test "$opal_memory_patcher_happy" == "yes"], [
        AC_CHECK_FUNCS([__curbrk])
	AC_CHECK_HEADERS([linux/mman.h sys/syscall.h])
	AC_CHECK_DECLS([__mmap], [], [], [#include <sys/mman.h>])
	AC_CHECK_FUNCS([__mmap])
	AC_CHECK_DECLS([__syscall], [], [], [#include <sys/syscall.h>])
	AC_CHECK_FUNCS([__syscall])
        $1], [$2])

    AC_CONFIG_FILES([opal/mca/memory/patcher/Makefile])
])
