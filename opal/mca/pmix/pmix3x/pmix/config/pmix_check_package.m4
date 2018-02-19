# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# _PMIX_CHECK_PACKAGE_HEADER(prefix, header, dir-prefix,
#                            [action-if-found], [action-if-not-found],
#                            includes)
# --------------------------------------------------------------------
AC_DEFUN([_PMIX_CHECK_PACKAGE_HEADER], [
    # This is stolen from autoconf to peek under the covers to get the
    # cache variable for the library check.  one should not copy this
    # code into other places unless you want much pain and suffering
    AS_VAR_PUSHDEF([pmix_Header], [ac_cv_header_$2])

    # so this sucks, but there's no way to get through the progression
    # of header includes without killing off the cache variable and trying
    # again...
    unset pmix_Header
    pmix_check_package_header_happy="no"

    # get rid of the trailing slash(es)
    hdir_prefix=$(echo $3 | sed -e 'sX/*$XXg')

    AS_IF([test "$hdir_prefix" = "" || \
           test "$hdir_prefix" = "/usr" || \
           test "$hdir_prefix" = "/usr/local"],
           [ # try as is...
            AC_VERBOSE([looking for header without includes])
            AC_CHECK_HEADERS([$2], [pmix_check_package_header_happy="yes"], [])
            AS_IF([test "$pmix_check_package_header_happy" = "no"],
                  [# no go on the as is - reset the cache and try again
                   unset pmix_Header])])

    AS_IF([test "$pmix_check_package_header_happy" = "no"],
          [AS_IF([test "$hdir_prefix" != ""],
                 [$1_CPPFLAGS="$$1_CPPFLAGS -I$hdir_prefix"
                  CPPFLAGS="$CPPFLAGS -I$hdir_prefix"
                  AC_VERBOSE([looking for header in $hdir_prefix])
                  AC_CHECK_HEADERS([$2], [pmix_check_package_header_happy="yes"], [], [$6])
                  AS_IF([test "$pmix_check_package_header_happy" = "no"],
                        [unset pmix_Header
                         $1_CPPFLAGS="$$1_CPPFLAGS -I$hdir_prefix/include"
                         CPPFLAGS="$CPPFLAGS -I$hdir_prefix/include"
                         AC_VERBOSE([looking for header in $hdir_prefix/include])
                         AC_CHECK_HEADERS([$2], [pmix_check_package_header_happy="yes"], [], [$6])])])])

    AS_IF([test "$pmix_check_package_header_happy" = "yes"],
          [$4], [$5])

    unset pmix_check_package_header_happy

    AS_VAR_POPDEF([pmix_Header])dnl
])


# _PMIX_CHECK_PACKAGE_LIB(prefix, library, function, extra-libraries,
#                         dir-prefix, libdir,
#                         [action-if-found], [action-if-not-found]])
# --------------------------------------------------------------------
AC_DEFUN([_PMIX_CHECK_PACKAGE_LIB], [
    # This is stolen from autoconf to peek under the covers to get the
    # cache variable for the library check.  one should not copy this
    # code into other places unless you want much pain and suffering
    AS_LITERAL_IF([$2],
                  [AS_VAR_PUSHDEF([pmix_Lib], [ac_cv_lib_$2_$3])],
                  [AS_VAR_PUSHDEF([pmix_Lib], [ac_cv_lib_$2''_$3])])dnl

    # see comment above
    unset pmix_Lib
    pmix_check_package_lib_happy="no"

    # get rid of the trailing slash(es)
    libdir_prefix=$(echo $6 | sed -e 'sX/*$XXg')

    AS_IF([test "$libdir_prefix" != ""],
          [# libdir was specified - search only there
           $1_LDFLAGS="$$1_LDFLAGS -L$libdir_prefix"
           LDFLAGS="$LDFLAGS -L$libdir_prefix"
           AC_SEARCH_LIBS([$3], [$2],
                        [pmix_check_package_lib_happy="yes"],
                        [pmix_check_package_lib_happy="no"], [$4])
           AS_IF([test "$pmix_check_package_lib_happy" = "no"],
                 [LDFLAGS="$pmix_check_package_$1_save_LDFLAGS"
                  $1_LDFLAGS="$pmix_check_package_$1_orig_LDFLAGS"
                  unset pmix_Lib])],
          [ # libdir was not specified - go through search path
            # get rid of the trailing slash(es)
            libdir_prefix=$(echo $5 | sed -e 'sX/*$XXg')

            # first try standard locations as otherwise our
            # searches with libdir_prefix locations might come
            # back positive and unnecessarily add an LDFLAG
            AC_VERBOSE([looking for library without search path])
            AC_SEARCH_LIBS([$3], [$2],
                           [pmix_check_package_lib_happy="yes"],
                           [pmix_check_package_lib_happy="no"], [$4])
            AS_IF([test "$pmix_check_package_lib_happy" = "no"],
                  [ # no go on the as is..  see what happens later...
                   LDFLAGS="$pmix_check_package_$1_save_LDFLAGS"
                   $1_LDFLAGS="$pmix_check_package_$1_orig_LDFLAGS"
                   unset pmix_Lib])

           AS_IF([test "$pmix_check_package_lib_happy" = "no"],
           # if we didn't find it, check the libdir_prefix/lib64 directory
               [AS_IF([test "$libdir_prefix" != "" && \
                       test "$libdir_prefix" != "/usr" && \
                       test "$libdir_prefix" != "/usr/local"],
                    [$1_LDFLAGS="$$1_LDFLAGS -L$libdir_prefix/lib64"
                     LDFLAGS="$LDFLAGS -L$libdir_prefix/lib64"
                     AC_VERBOSE([looking for library in $libdir_prefix/lib64])
                     AC_SEARCH_LIBS([$3], [$2],
                               [pmix_check_package_lib_happy="yes"],
                               [pmix_check_package_lib_happy="no"], [$4])
                     AS_IF([test "$pmix_check_package_lib_happy" = "no"],
                         [ # no go on the as is..  see what happens later...
                          LDFLAGS="$pmix_check_package_$1_save_LDFLAGS"
                          $1_LDFLAGS="$pmix_check_package_$1_orig_LDFLAGS"
                          unset pmix_Lib])])])

           AS_IF([test "$pmix_check_package_lib_happy" = "no"],
           # if we still haven't found it, check the libdir_prefix/lib directory
               [AS_IF([test "$libdir_prefix" != "" && \
                       test "$libdir_prefix" != "/usr" && \
                       test "$libdir_prefix" != "/usr/local"],
                    [$1_LDFLAGS="$$1_LDFLAGS -L$libdir_prefix/lib"
                     LDFLAGS="$LDFLAGS -L$libdir_prefix/lib"
                     AC_VERBOSE([looking for library in $libdir_prefix/lib])
                     AC_SEARCH_LIBS([$3], [$2],
                               [pmix_check_package_lib_happy="yes"],
                               [pmix_check_package_lib_happy="no"], [$4])
                     AS_IF([test "$pmix_check_package_lib_happy" = "no"],
                         [ # no go on the as is..  see what happens later...
                          LDFLAGS="$pmix_check_package_$1_save_LDFLAGS"
                          $1_LDFLAGS="$pmix_check_package_$1_orig_LDFLAGS"
                          unset pmix_Lib])])])
         ])


    AS_IF([test "$pmix_check_package_lib_happy" = "yes"],
          [$1_LIBS="-l$2 $4"
           $7], [$8])
    AS_IF([test "$pmix_check_package_lib_happy" = "yes"],
          [ # The result of AC SEARCH_LIBS is cached in $ac_cv_search_[function]
           AS_IF([test "$ac_cv_search_$3" != "no" &&
                  test "$ac_cv_search_$3" != "none required"],
                 [$1_LIBS="$ac_cv_search_$3 $4"],
                 [$1_LIBS="$4"])
           $7],
          [$8])

    AS_VAR_POPDEF([pmix_Lib])dnl
])


# PMIX_CHECK_PACKAGE(prefix,
#                    header,
#                    library,
#                    function,
#                    extra-libraries,
#                    dir-prefix,
#                    libdir-prefix,
#                    [action-if-found], [action-if-not-found],
#                    includes)
# -----------------------------------------------------------
# check for package defined by header and libs, and probably
# located in dir-prefix, possibly with libs in libdir-prefix.
# Both dir-prefix and libdir-prefix can be empty.  Will set
# prefix_{CPPFLAGS, LDFLAGS, LIBS} as needed
AC_DEFUN([PMIX_CHECK_PACKAGE],[
    pmix_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    pmix_check_package_$1_save_LDFLAGS="$LDFLAGS"
    pmix_check_package_$1_save_LIBS="$LIBS"

    pmix_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    pmix_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    pmix_check_package_$1_orig_LIBS="$$1_LIBS"

    _PMIX_CHECK_PACKAGE_HEADER([$1], [$2], [$6],
          [_PMIX_CHECK_PACKAGE_LIB([$1], [$3], [$4], [$5], [$6], [$7],
                [pmix_check_package_happy="yes"],
                [pmix_check_package_happy="no"])],
          [pmix_check_package_happy="no"],
          [$10])

    AS_IF([test "$pmix_check_package_happy" = "yes"],
          [$8],
          [$1_CPPFLAGS="$pmix_check_package_$1_orig_CPPFLAGS"
           $1_LDFLAGS="$pmix_check_package_$1_orig_LDFLAGS"
           $1_LIBS="$pmix_check_package_$1_orig_LIBS"
           $9])

    CPPFLAGS="$pmix_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$pmix_check_package_$1_save_LDFLAGS"
    LIBS="$pmix_check_package_$1_save_LIBS"
])
