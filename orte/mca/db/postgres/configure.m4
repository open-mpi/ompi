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

# MCA_db_postgres_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_db_postgres_CONFIG], [
    AC_CONFIG_FILES([orte/mca/db/postgres/Makefile])

    AC_REQUIRE([OPAL_CHECK_POSTGRES])

    # do not build if support not requested
    AS_IF([test "$orte_check_postgres_happy" == "yes"],
          [orte_db_postgres_check_save_CPPFLAGS=$CPPFLAGS
           orte_db_postgres_check_save_LDFLAGS=$LDFLAGS
           orte_db_postgres_check_save_LIBS=$LIBS
           OMPI_CHECK_PACKAGE([db_postgres],
                              [libpq-fe.h],
                              [pq],
                              [PQconnectdb],
                              [],
                              [$orte_postgres_incdir],
                              [],
                              [$1],
                              [AC_MSG_WARN([Postgres database support requested])
                               AC_MSG_WARN([but required library or header not found])
                               AC_MSG_ERROR([Cannot continue])
                               $2])
           CPPFLAGS=$orte_db_postgres_check_save_CPPFLAGS
           LDFLAGS=$orte_db_postgres_check_save_LDFLAGS
           LIBS=$orte_db_postgres_check_save_LIBS],
          [$2])

    AC_SUBST(db_postgres_CPPFLAGS)
    AC_SUBST(db_postgres_LDFLAGS)
    AC_SUBST(db_postgres_LIBS)
])dnl
