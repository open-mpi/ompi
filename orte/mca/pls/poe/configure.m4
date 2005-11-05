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

# MCA_pls_poe_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------

AC_DEFUN([MCA_pls_poe_CONFIG],[
    # POE is only supported on AIX.  We only need executables (no
    # header files or libraries), but those can be found (or not) at
    # run-time.  So if we're on AIX, build this component.
    AC_MSG_CHECKING([if on AIX])
    case $host_os in
    aix3* | aix4* | aix5*)
        happy=yes
        ;;
    *)
        happy=no
        ;;
    esac
    AC_MSG_RESULT([$happy])
    AS_IF([test "$happy" = "yes"], [$1], [$2])
])
