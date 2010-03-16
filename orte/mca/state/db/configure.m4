dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_state_db_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_state_db_CONFIG], [
    # only build if db.h is found
    AC_CHECK_HEADERS([db.h], [$1], [$2], [AC_INCLUDES_DEFAULT])
])dnl
