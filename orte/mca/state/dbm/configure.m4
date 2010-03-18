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
    # only build if ndbm.h and its library are found
    OMPI_CHECK_PACKAGE([state_dbm],
                       [ndbm.h],
                       [dbm],
                       [dbm_open],
                       [],
                       [],
                       [],
                       [$1],
                       [$2])])

    AC_SUBST(state_dbm_CPPFLAGS)
    AC_SUBST(state_dbm_LDFLAGS)
    AC_SUBST(state_dbm_LIBS)
])dnl
