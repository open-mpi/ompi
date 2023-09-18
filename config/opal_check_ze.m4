dnl
dnl Copyright (C) 2022-2023 Advanced Micro Devices, Inc. All rights reserved.
dnl Copyright (c) 2023     Triad National Security, LLC. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# OPAL_CHECK_ZE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if Intel ZE support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found


#
# Check for ZE  support
#
AC_DEFUN([OPAL_CHECK_ZE],[

     OPAL_VAR_SCOPE_PUSH([opal_check_ze_happy ze_save_CPPFLAGS ze_save_LDFLAGS ze_save_LIBS ze_CPPFLAGS ze_LDFLAGS ze_LIBS])

     ze_save_CPPFLAGS="$CPPFLAGS"
     ze_save_LDFLAGS="$LDFLAGS"
     ze_save_LIBS="$LIBS"

     # Get some configuration information
     AC_ARG_WITH([ze],
        [AS_HELP_STRING([--with-ze(=DIR)],
        [Build Intel ZE support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])


     AS_IF([ test -n "$with_ze" && test "$with_ze" = "yes" ],
           [ with_ze="/opt/ze"] )

     m4_define([ze_pkgconfig_module], [level-zero])
     OAC_CHECK_PACKAGE([ze],
                       [$1],
                       [level_zero/ze_api.h],
                       [ze_loader],
		       [zeInit],
                       [opal_check_ze_happy="yes"],
                       [opal_check_ze_happy="no"])

     LDFLAGS="$ze_save_LDFLAGS"
     LIBS="$ze_save_LIBS"
     OPAL_APPEND([CPPFLAGS], [${$1_CPPFLAGS}] )
     OPAL_APPEND([LDFLAGS], [${$1_LDFLAGS}] )
     OPAL_APPEND([LIBS], [${$1_LIBS}] )

     AS_IF([ test "$opal_check_ze_happy" = "no" ],
           [ CPPFLAGS="$ze_save_CPPFLAGS"])

     AS_IF([ test "$opal_check_ze_happy" = "yes" ],
           [ AC_DEFINE_UNQUOTED([OPAL_ZE_SUPPORT], [1], [Enable Intel ZE support])
             ZE_SUPPORT=1 ],
           [ AC_DEFINE_UNQUOTED([OPAL_ZE_SUPPORT], [0], [Disable Intel ZE support])
             ZE_SUPPORT=0 ])

     AS_IF([ test "$opal_check_ze_happy" = "yes" ],
            [$2],
            [AS_IF([test -n "$with_ze" && test "$with_ze" != "no"],
                   [AC_MSG_ERROR([Intel ZE support requested but not found.  Aborting])])
            $3])

     AM_CONDITIONAL([OPAL_ze_support], [test "$opal_check_ze_happy" = "yes"])
     OPAL_VAR_SCOPE_POP
])
