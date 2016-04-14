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

    OPAL_VAR_SCOPE_PUSH([memory_patcher_have___curbrk memory_patcher_have___mmap memory_patcher_have___syscall memory_patcher_have___mmap_prototype memory_patcher_have___syscall_prototype])

    memory_patcher_have___curbrk=0
    memory_patcher_have___mmap=0
    memory_patcher_have___mmap_prototype=0
    memory_patcher_have___syscall=0
    memory_patcher_have___syscall_prototype=0

    AC_MSG_CHECKING([for __curbrk symbol])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([extern char *__curbrk;],[char *tmp = __curbrk;])],
                   [AC_MSG_RESULT([yes])
                    memory_patcher_have___curbrk=1],
                   [AC_MSG_RESULT([no])])
    AC_DEFINE_UNQUOTED([OPAL_MEMORY_PATCHER_HAVE___CURBRK], [$memory_patcher_have___curbrk],
                       [Whether the glibc __curbrk exists])

    AC_MSG_CHECKING([whether __mmap prototype exists])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/mman.h>],[char *tmp = __mmap (NULL, 0, 0, 0, 0, 0);])],
                   [AC_MSG_RESULT([yes])
                    memory_patcher_have___mmap_prototype=1],
                   [AC_MSG_RESULT([no])])
    AC_DEFINE_UNQUOTED([OPAL_MEMORY_PATCHER_HAVE___MMAP_PROTO], [$memory_patcher_have___mmap_prototype],
                       [Whether the internal __mmap call has a prototype])

    AC_MSG_CHECKING([whether __mmap symbol exists])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([void *__mmap ();],[char *tmp = __mmap ();])],
                   [AC_MSG_RESULT([yes])
                    memory_patcher_have___mmap=1],
                   [AC_MSG_RESULT([no])])
    AC_DEFINE_UNQUOTED([OPAL_MEMORY_PATCHER_HAVE___MMAP], [$memory_patcher_have___mmap],
                       [Whether the internal __mmap call exists])

    AC_MSG_CHECKING([whether __syscall prototype exists])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <sys/syscall.h>],[char *tmp = __syscall (SYS_mmap, NULL);])],
                   [AC_MSG_RESULT([yes])
                    memory_patcher_have___syscall_prototype=1],
                   [AC_MSG_RESULT([no])])
    AC_DEFINE_UNQUOTED([OPAL_MEMORY_PATCHER_HAVE___SYSCALL_PROTO], [$memory_patcher_have___syscall_prototype],
                       [Whether the internal __syscall call has a prototype])

    AC_MSG_CHECKING([whether __syscall symbol exists])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([void *__syscall ();],[char *tmp = __syscall ();])],
                   [AC_MSG_RESULT([yes])
                    memory_patcher_have___syscall=1],
                   [AC_MSG_RESULT([no])])
    AC_DEFINE_UNQUOTED([OPAL_MEMORY_PATCHER_HAVE___SYSCALL], [$memory_patcher_have___syscall],
                       [Whether the internal __syscall call exists])

    [$1]

    OPAL_VAR_SCOPE_POP
])
