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


# OMPI_CHECK_ALPS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_ALPS],[
	AC_ARG_WITH([alps],
	    [AC_HELP_STRING([--with-alps],
		    [Build ALPS scheduler component (default: no)])])
	if test "$with_alps" = "yes" ; then
	    ompi_check_alps_happy="yes"
	fi
	AS_IF([test "$ompi_check_alps_happy" = "yes"], 
	    [$2], 
	    [$3])
	])
