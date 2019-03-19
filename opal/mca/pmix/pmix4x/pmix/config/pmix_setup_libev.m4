# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2017-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_libev_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([PMIX_LIBEV_CONFIG],[
    PMIX_VAR_SCOPE_PUSH([pmix_libev_dir pmix_libev_libdir pmix_libev_standard_header_location pmix_libev_standard_lib_location])

    AC_ARG_WITH([libev],
                [AC_HELP_STRING([--with-libev=DIR],
                                [Search for libev headers and libraries in DIR ])])
    PMIX_CHECK_WITHDIR([libev], [$with_libev], [include/event.h])

    AC_ARG_WITH([libev-libdir],
                [AC_HELP_STRING([--with-libev-libdir=DIR],
                                [Search for libev libraries in DIR ])])
    PMIX_CHECK_WITHDIR([libev-libdir], [$with_livev_libdir], [libev.*])

    pmix_libev_support=0

    AS_IF([test -n "$with_libev" && test "$with_libev" != "no"],
          [AC_MSG_CHECKING([for libev in])
           pmix_check_libev_save_CPPFLAGS="$CPPFLAGS"
           pmix_check_libeve_save_LDFLAGS="$LDFLAGS"
           pmix_check_libev_save_LIBS="$LIBS"
           if test "$with_libev" != "yes"; then
               pmix_libev_dir=$with_libev/include
               pmix_libev_standard_header_location=no
               pmix_libev_standard_lib_location=no
               AS_IF([test -z "$with_libev_libdir" || test "$with_libev_libdir" = "yes"],
                     [if test -d $with_libev/lib; then
                          pmix_libev_libdir=$with_libev/lib
                      elif test -d $with_libev/lib64; then
                          pmix_libev_libdir=$with_libev/lib64
                      else
                          AC_MSG_RESULT([Could not find $with_libev/lib or $with_libev/lib64])
                          AC_MSG_ERROR([Can not continue])
                      fi
                      AC_MSG_RESULT([$pmix_libev_dir and $pmix_libev_libdir])],
                     [AC_MSG_RESULT([$with_libev_libdir])])
           else
               AC_MSG_RESULT([(default search paths)])
               pmix_libev_standard_header_location=yes
               pmix_libev_standard_lib_location=yes
           fi
           AS_IF([test ! -z "$with_libev_libdir" && test "$with_libev_libdir" != "yes"],
                 [pmix_libev_libdir="$with_libev_libdir"
                  pmix_libev_standard_lib_location=no])

           PMIX_CHECK_PACKAGE([pmix_libev],
                              [event.h],
                              [ev],
                              [event_base_new],
                              [],
                              [$pmix_libev_dir],
                              [$pmix_libev_libdir],
                              [pmix_libev_support=1],
                              [pmix_libev_support=0])
           CPPFLAGS="$pmix_check_libev_save_CPPFLAGS"
           LDFLAGS="$pmix_check_libev_save_LDFLAGS"
           LIBS="$pmix_check_libev_save_LIBS"])

    AS_IF([test $pmix_libev_support -eq 1],
          [LIBS="$LIBS $pmix_libev_LIBS"

           AS_IF([test "$pmix_libev_standard_header_location" != "yes"],
                 [CPPFLAGS="$CPPFLAGS $pmix_libev_CPPFLAGS"])
           AS_IF([test "$pmix_libev_standard_lib_location" != "yes"],
                 [LDFLAGS="$LDFLAGS $pmix_libev_LDFLAGS"])])

    AC_MSG_CHECKING([will libev support be built])
    if test $pmix_libev_support -eq 1; then
        AC_MSG_RESULT([yes])
        PMIX_EVENT_HEADER="<event.h>"
        AC_DEFINE_UNQUOTED([PMIX_EVENT_HEADER], [$PMIX_EVENT_HEADER],
                           [Location of event.h])
        PMIX_SUMMARY_ADD([[External Packages]],[[libev]],[libev],[$pmix_libev_dir])
    else
        AC_MSG_RESULT([no])
    fi

    AC_DEFINE_UNQUOTED([PMIX_HAVE_LIBEV], [$pmix_libev_support], [Whether we are building against libev])

    PMIX_VAR_SCOPE_POP
])dnl
