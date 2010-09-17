dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_db_dbm_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_db_dbm_CONFIG], [
    AC_CONFIG_FILES([orte/mca/db/dbm/Makefile])

    # only build if ndbm.h and its library are found
    OMPI_CHECK_PACKAGE([db_dbm],
                       [ndbm.h],
                       [dbm],
                       [dbm_open],
                       [],
                       [],
                       [],
                       [$1],
                       [$2])])

    AC_SUBST(db_dbm_CPPFLAGS)
    AC_SUBST(db_dbm_LDFLAGS)
    AC_SUBST(db_dbm_LIBS)
])dnl
