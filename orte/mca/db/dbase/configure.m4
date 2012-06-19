dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
dnl Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
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

    # do not build if rte is disabled
    AS_IF([test "$orte_without_full_support" = 0],
          [OMPI_CHECK_PACKAGE([db_db],
                              [db.h],
                              [db],
                              [dbopen],
                              [],
                              [],
                              [],
                              [$1],
                              [$2])],
          [$2])

    AC_SUBST(db_dbase_CPPFLAGS)
    AC_SUBST(db_dbase_LDFLAGS)
    AC_SUBST(db_dbase_LIBS)
])dnl
