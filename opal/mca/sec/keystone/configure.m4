dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# MCA_sec_keystone_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_sec_keystone_CONFIG], [
    AC_CONFIG_FILES([opal/mca/sec/keystone/Makefile])

    AC_ARG_WITH([keystone],
                [AC_HELP_STRING([--with-keystone],
                                [Build keystone support (default: no)])],
	                        [], with_keystone=no)

    # do not build if support not requested
    AC_MSG_CHECKING([want keystone security])
    AS_IF([test "$with_keystone" != "no"],
          [AC_MSG_RESULT([yes])
           AS_IF([test ! -z "$with_keystone" && test "$with_keystone" != "yes"],
                 [opal_check_keystone_dir="$with_keystone"])
           OPAL_CHECK_PACKAGE([sec_keystone],
                              [curl/curl.h],
                              [curl],
                              [curl_easy_init],
                              [],
                              [],
                              [],
                              [$1],
                              [AC_MSG_WARN([KEYSTONE SUPPORT REQUESTED])
                               AC_MSG_WARN([BUT REQUIRED CURL LIBRARY OR HEADER NOT FOUND])
                               AC_MSG_ERROR([CANNOT CONTINUE])
                               $2])],
          [AC_MSG_RESULT([no])
           $2])

    AC_SUBST(sec_keystone_CPPFLAGS)
    AC_SUBST(sec_keystone_LDFLAGS)
    AC_SUBST(sec_keystone_LIBS)
])dnl
