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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_backtrace_darwin_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_backtrace_darwin_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_backtrace_darwin_CONFIG],[
    AC_CHECK_FUNCS([vm_read_overwrite], 
                   [backtrace_darwin_happy="yes"],
                   [backtrace_darwin_happy="no"])

    if test "$backtrace_darwin_happy" = "yes" ; then 
       if test "$OMPI_C_GCC_INLINE_ASSEMBLY" = "0" ; then
           backtrace_darwin_happy="no"
       fi
    fi

    AS_IF([test "$backtrace_darwin_happy" = "yes"], 
          [$1], [$2])
])
