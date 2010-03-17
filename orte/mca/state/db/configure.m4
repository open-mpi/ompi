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
    # only build if db.h and its corresponding library are found
    OMPI_CHECK_PACKAGE([state_db],
                       [db.h],
                       [db],
                       [dbopen],
                       [],
                       [],
                       [],
                       [$1],
                       [$2])])
])dnl
