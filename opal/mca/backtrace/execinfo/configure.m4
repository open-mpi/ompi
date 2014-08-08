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
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_opal_backtrace_execinfo_PRIORITY], [30])

AC_DEFUN([MCA_opal_backtrace_execinfo_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_backtrace_execinfo_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_backtrace_execinfo_CONFIG],[
    AC_CONFIG_FILES([opal/mca/backtrace/execinfo/Makefile])

    AC_CHECK_HEADERS([execinfo.h])
    # FreeBSD has backtrace in -lexecinfo, usually in libc
    OPAL_SEARCH_LIBS_COMPONENT([backtrace_execinfo], [backtrace], [execinfo],
                   [backtrace_execinfo_happy="yes"],
                   [backtrace_execinfo_happy="no"])

    AS_IF([test "$backtrace_execinfo_happy" = "yes"], 
          [$1], [$2])
])
