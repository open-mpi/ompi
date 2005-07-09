# -*- autoconf -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

AM_MAKEVAR([OBJC], [AC_PROG_OBJC])

AC_DEFUN([AC_PROG_OBJC], [
    AC_ARG_VAR([OBJC], [Objective C compiler command])
    AC_ARG_VAR([OBJCFLAGS], [Objective C compiler flags])
    AC_CHECK_TOOLS(OBJC, [$CCC m4_default([$1], [gcc cc objc])], gcc)
    _AM_DEPENDENCIES([OBJC])
])dnl
