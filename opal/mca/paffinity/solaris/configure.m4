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
# Copyright (c) 2007      Cisco, Inc. All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_paffinity_solaris_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_paffinity_solaris_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([paff_solaris_happy])
    #check to see if we have <sys/procset.h>
    AC_CHECK_HEADER([sys/procset.h], [paff_solaris_happy=yes], [paff_solaris_happy=no])

    if test "$paff_solaris_happy" = "yes"; then
        # check for processor_bind()
        AC_CHECK_FUNC([processor_bind],[paff_solaris_happy=yes],[paff_solaris_happy=no])
    fi

    if test "$paff_solaris_happy" = "yes"; then
       # check for whether header has P_PID defined
       AC_MSG_CHECKING([if P_PID is defined])
       AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/procset.h>]], [[int i = P_PID;]])],
                         [paff_solaris_happy=yes],[paff_solaris_happy=no])
       AC_MSG_RESULT([$paff_solaris_happy ])
    fi

    AS_IF([test "$paff_solaris_happy" = "yes"], [$1], [$2])
    OMPI_VAR_SCOPE_POP
])dnl

