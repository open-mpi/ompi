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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# ORTE_CHECK_ALPS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_ALPS],[
        # require that we check for pmi support request first so
        # we can get the static library ordering correct
        AC_REQUIRE([ORTE_CHECK_PMI])

	AC_ARG_WITH([alps],
	    [AC_HELP_STRING([--with-alps],
		    [Build ALPS scheduler component (default: no)])])
	if test "$with_alps" = "yes" ; then
	    orte_check_alps_happy="yes"
            # if pmi support is requested, then ORTE_CHECK_PMI will
            # have added the -lpmi flag to LIBS. We then need to
            # add a couple of alps libs to support static builds
            if test "$orte_enable_pmi" = 1 ; then
                LDFLAGS="$LDFLAGS -L/usr/lib/alps"
                LIBS="$LIBS -lalpslli -lalpsutil"
            fi
	fi
	AS_IF([test "$orte_check_alps_happy" = "yes"], 
	    [$2], 
	    [$3])
	])
