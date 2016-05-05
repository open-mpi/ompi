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

# MCA_patcher_linux_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_patcher_linux_CONFIG],[
    AC_CONFIG_FILES([opal/mca/patcher/linux/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_patcher_linux_CPPFLAGS_save])

    opal_patcher_linux_happy=no
    if test $OPAL_ENABLE_DLOPEN_SUPPORT = 1 ; then
        # Only enable on Linux for now. In the future this component might
        # be modified to work on FreeBSD.
        case $host in
            *-linux*)
                opal_patcher_linux_happy=yes;
                ;;
        esac

        if test $opal_patcher_linux_happy = yes ; then
            OPAL_CHECK_PACKAGE([patcher_linux], [dlfcn.h], [dl], [dl_iterate_phdr], [], [], [],
                               [],[opal_patcher_linux_happy=no])
            AC_CHECK_HEADERS([elf.h],[],[opal_patcher_linux_happy=no])
	    AC_CHECK_HEADERS([sys/auxv.h])
        fi
    fi

    # this component can not be used until we can determine a way to hook munmap, etc inside
    # glibc. this is needed to catch munmap called by free
    opal_patcher_linux_happy=no

    AS_IF([test $opal_patcher_linux_happy = yes], [$1], [$2])
    OPAL_VAR_SCOPE_POP
])
