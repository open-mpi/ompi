dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
dnl Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_db_sqlite_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_db_sqlite_CONFIG], [
    AC_CONFIG_FILES([orte/mca/db/sqlite/Makefile])

    AC_ARG_WITH([sqlite3],
                [AC_HELP_STRING([--with-sqlite3],
                                [Build sqlite3 support (default: no)])],
	                        [], with_sqlite3=no)

    # do not build if rte is disabled or support not requested
    AS_IF([test "$with_sqlite3" != "no"],
          [AS_IF([test ! -z "$with_sqlite3" -a "$with_sqlite3" != "yes"],
                 [orte_check_sqlite3_dir="$with_sqlite3"])
           OPAL_CHECK_PACKAGE([db_sqlite],
                              [sqlite3.h],
                              [sqlite3],
                              [sqlite3_open],
                              [],
                              [$orte_check_sqlite3_dir],
                              [],
                              [$1],
                              [$2])],
          [$2])

    AC_SUBST(db_sqlite_CPPFLAGS)
    AC_SUBST(db_sqlite_LDFLAGS)
    AC_SUBST(db_sqlite_LIBS)
])dnl
