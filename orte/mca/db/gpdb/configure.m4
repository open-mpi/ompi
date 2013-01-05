dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_db_gpdb_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_db_gpdb_CONFIG], [
    AC_CONFIG_FILES([orte/mca/db/gpdb/Makefile])

    AC_ARG_WITH([gpdb],
                [AC_HELP_STRING([--with-gpdb],
                                [Build gpdb support (default: no)])],
	                        [], with_gpdb=no)

    # do not build if rte is disabled or support not requested
    AS_IF([test "$orte_without_full_support" = 0 -a "$with_gpdb" != "no"],
          [AS_IF([test ! -z "$with_gpdb" -a "$with_gpdb" != "yes"],
                 [orte_check_gpdb_dir="$with_gpdb"])
           OMPI_CHECK_PACKAGE([db_gpdb],
                              [gpdb.h],
                              [gpdb],
                              [gpdb_open],
                              [],
                              [$orte_check_gpdb_dir],
                              [],
                              [$1],
                              [$2])],
          [$2])

    AC_SUBST(db_gpdb_CPPFLAGS)
    AC_SUBST(db_gpdb_LDFLAGS)
    AC_SUBST(db_gpdb_LIBS)
])dnl
