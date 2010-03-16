dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_state_dbm_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_state_dbm_CONFIG], [
    # only build if ndbm.h is found
    AC_CHECK_HEADERS([ndbm.h], [$1], [$2], [AC_INCLUDES_DEFAULT])
])dnl
