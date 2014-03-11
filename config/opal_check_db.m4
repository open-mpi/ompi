# -*- shell-script -*-
#
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_POSTGRES],[
    AC_ARG_WITH([postgres],
                [AC_HELP_STRING([--with-postgres(=DIR)],
                                [Build Postgres support, optionally adding DIR to the search path (default: no)])],
	                        [], with_postgres=no)

    AC_MSG_CHECKING([if Postgres support requested])
    AS_IF([test "$with_postgres" = "no" -o "x$with_postgres" = "x"],
      [opal_check_postgres_happy=no
       AC_MSG_RESULT([no])],
      [AS_IF([test "$with_postgres" = "yes"],
             [AS_IF([test "x`ls /usr/include/libpq-fe.h 2> /dev/null`" = "x"],
                    [AC_MSG_RESULT([not found in standard location])
                     AC_MSG_WARN([Expected file /usr/include/libpq-fe.h not found])
                     AC_MSG_ERROR([Cannot continue])],
                    [AC_MSG_RESULT([found])
                     opal_check_postgres_happy=yes
                     opal_postgres_incdir="/usr/include"])],
             [AS_IF([test ! -d "$with_postgres"],
                    [AC_MSG_RESULT([not found])
                     AC_MSG_WARN([Directory $with_postgres not found])
                     AC_MSG_ERROR([Cannot continue])],
                    [AS_IF([test "x`ls $with_postgres/include/libpq-fe.h 2> /dev/null`" = "x"],
                           [AS_IF([test "x`ls $with_postgres/libpq-fe.h 2> /dev/null`" = "x"],
                                  [AC_MSG_RESULT([not found])
                                   AC_MSG_WARN([Could not find libpq-fe.h in $with_postgres/include or $with_postgres])
                                   AC_MSG_ERROR([Cannot continue])],
                                  [opal_check_postgres_happy=yes
                                   opal_postgres_incdir=$with_postgres
                                   AC_MSG_RESULT([found ($with_postgres/libpq-fe.h)])])],
                           [opal_check_postgres_happy=yes
                            opal_postgres_incdir="$with_postgres/include"
                            AC_MSG_RESULT([found ($opal_postgres_incdir/libpq-fe.h)])])])])])
])

# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_MYSQL],[
    AC_ARG_WITH([mysql],
                [AC_HELP_STRING([--with-mysql(=DIR)],
                                [Build Mysql support, optionally adding DIR to the search path (default: no)])],
	                        [], with_mysql=no)

    AC_MSG_CHECKING([if Mysql support requested])
    AS_IF([test "$with_mysql" = "no" -o "x$with_mysql" = "x"],
      [opal_check_mysql_happy=no
       AC_MSG_RESULT([no])],
      [AS_IF([test "$with_mysql" = "yes"],
             [AS_IF([test "x`ls /usr/include/mysql/mysql.h 2> /dev/null`" = "x"],
                    [AC_MSG_RESULT([not found in standard location])
                     AC_MSG_WARN([Expected file /usr/include/mysql/mysql.h not found])
                     AC_MSG_ERROR([Cannot continue])],
                    [AC_MSG_RESULT([found])
                     opal_check_mysql_happy=yes
                     opal_mysql_incdir="/usr/include/mysql"])],
             [AS_IF([test ! -d "$with_mysql"],
                    [AC_MSG_RESULT([not found])
                     AC_MSG_WARN([Directory $with_mysql not found])
                     AC_MSG_ERROR([Cannot continue])],
                    [AS_IF([test "x`ls $with_mysql/include/mysql.h 2> /dev/null`" = "x"],
                           [AS_IF([test "x`ls $with_mysql/mysql.h 2> /dev/null`" = "x"],
                                  [AC_MSG_RESULT([not found])
                                   AC_MSG_WARN([Could not find mysql.h in $with_mysql/include or $with_mysql])
                                   AC_MSG_ERROR([Cannot continue])],
                                  [opal_check_mysql_happy=yes
                                   opal_mysql_incdir=$with_mysql
                                   AC_MSG_RESULT([found ($with_mysql/mysql.h)])])],
                           [opal_check_mysql_happy=yes
                            opal_mysql_incdir="$with_mysql/include"
                            AC_MSG_RESULT([found ($opal_mysql_incdir/mysql.h)])])])])])
])
