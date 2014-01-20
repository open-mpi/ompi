dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
dnl Copyright (c) 2013      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_db_postgres_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_db_postgres_CONFIG], [
    AC_CONFIG_FILES([opal/mca/db/postgres/Makefile])

    AC_ARG_WITH([postgres],
                [AC_HELP_STRING([--with-postgres],
                                [Build postgres support (default: no)])],
	                        [], with_postgres=no)

    # do not build if support not requested
    AS_IF([test "$with_postgres" != "no"],
          [AS_IF([test ! -z "$with_postgres" -a "$with_postgres" != "yes"],
                 [opal_check_postgres_dir="$with_postgres"])
           OMPI_CHECK_PACKAGE([db_postgres],
                              [libpq-fe.h],
                              [pq],
                              [PQconnectdb],
                              [],
                              [$opal_check_postgres_dir],
                              [],
                              [$1],
                              [AC_MSG_WARN([POSTGRES DATABASE SUPPORT REQUESTED])
                               AC_MSG_WARN([BUT REQUIRED LIBRARY OR HEADER NOT FOUND])
                               AC_MSG_ERROR([CANNOT CONTINUE])
                               $2])],
          [$2])

    AC_SUBST(db_postgres_CPPFLAGS)
    AC_SUBST(db_postgres_LDFLAGS)
    AC_SUBST(db_postgres_LIBS)
])dnl
