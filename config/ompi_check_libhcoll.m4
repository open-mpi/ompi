dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2011      Mellanox Technologies. All rights reserved.
dnl Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_HCOLL(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if hcoll support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_HCOLL],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_hcoll_dir ompi_check_hcoll_libs ompi_check_hcoll_happy CPPFLAGS_save LDFLAGS_save LIBS_save])

    AC_ARG_WITH([hcoll],
        [AC_HELP_STRING([--with-hcoll(=DIR)],
             [Build hcoll (Mellanox Hierarchical Collectives) support, optionally adding
              DIR/include and DIR/lib or DIR/lib64 to the search path for headers and libraries])])

    AS_IF([test "$with_hcoll" != "no"],
          [ompi_check_hcoll_libs=hcoll
           AS_IF([test ! -z "$with_hcoll" && test "$with_hcoll" != "yes"],
                 [ompi_check_hcoll_dir=$with_hcoll])

           CPPFLAGS_save=$CPPFLAGS
           LDFLAGS_save=$LDFLAGS
           LIBS_save=$LIBS

           OPAL_LOG_MSG([$1_CPPFLAGS : $$1_CPPFLAGS], 1)
           OPAL_LOG_MSG([$1_LDFLAGS  : $$1_LDFLAGS], 1)
           OPAL_LOG_MSG([$1_LIBS     : $$1_LIBS], 1)

           OPAL_CHECK_PACKAGE([$1],
                              [hcoll/api/hcoll_api.h],
                              [$ompi_check_hcoll_libs],
                              [hcoll_get_version],
                              [],
                              [$ompi_check_hcoll_dir],
                              [],
                              [ompi_check_hcoll_happy="yes"],
                              [ompi_check_hcoll_happy="no"])

           AS_IF([test "$ompi_check_hcoll_happy" = "yes"],
                 [
                     CPPFLAGS=$coll_hcoll_CPPFLAGS
                     LDFLAGS=$coll_hcoll_LDFLAGS
                     LIBS=$coll_hcoll_LIBS
                     AC_CHECK_FUNCS(hcoll_context_free, [], [])
                 ],
                 [])

           CPPFLAGS=$CPPFLAGS_save
           LDFLAGS=$LDFLAGS_save
           LIBS=$LIBS_save],
          [ompi_check_hcoll_happy=no])

    AS_IF([test "$ompi_check_hcoll_happy" = "yes" && test "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([hcoll driver does not currently support progress threads.  Disabling HCOLL.])
           ompi_check_hcoll_happy="no"])

    AS_IF([test "$ompi_check_hcoll_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_hcoll" && test "$with_hcoll" != "no"],
                 [AC_MSG_ERROR([HCOLL support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])
