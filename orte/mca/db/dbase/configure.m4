dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_db_dbase_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_db_dbase_CONFIG], [
    AC_CONFIG_FILES([orte/mca/db/dbase/Makefile])

    # only build if db.h and its corresponding library are found
    OMPI_CHECK_PACKAGE([db_db],
                       [db.h],
                       [db],
                       [dbopen],
                       [],
                       [],
                       [],
                       [$1],
                       [$2])])

    AC_SUBST(db_dbase_CPPFLAGS)
    AC_SUBST(db_dbase_LDFLAGS)
    AC_SUBST(db_dbase_LIBS)
])dnl
