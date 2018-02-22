dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2012-2017 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl OPAL_DECLARE_PACKAGE(name,[help],[cppflagshelp],[ldflagshelp])
AC_DEFUN([OPAL_DECLARE_PACKAGE],[
    AC_ARG_WITH([$1], [AC_HELP_STRING([--with-$1=DIR], [$2])])
    AC_ARG_WITH([$1-cppflags], [AC_HELP_STRING([--with-$1-cppflags=FLAGS], [$3])])
    AC_ARG_WITH([$1-ldflags], [AC_HELP_STRING([--with-$1-ldflags=FLAGS], [$4])])
    AC_ARG_WITH([$1-libdir], [AC_HELP_STRING([--with-$1-libdir=DIR],
                                             [Deprecated, use --with-$1-ldflags instead])])
    AS_IF([test -n "$with_$1_libdir"],
          [AS_IF([test -n "$with_$1_ldflags"],
                 [AC_MSG_WARN([It is not possible to use both --with-$1-ldflags and --with-$1-libdir])
                  AC_MSG_ERROR([Cannot continue])],
                 [AC_MSG_WARN([--with_$1_libdir=DIR is deprecated, use --with-$1_ldflags=FLAGS instead])
                  with_$1_ldflags="-L$with_$1_libdir"])])
])

dnl --------------------------------------------------------------------
dnl _OPAL_CHECK_PACKAGE2_HEADER(package, prefix, header, includes,
dnl                             [action-if-found], [action-if-not-found])
dnl --------------------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_PACKAGE2_HEADER], [
    AS_VAR_PUSHDEF([opal_With], [with_$1])
    AS_VAR_PUSHDEF([opal_Cppflags], [with_$1_cppflags])

    AS_IF([test -n "$opal_Cppflags"],
          [$2_CPPFLAGS="$$2_CPPFLAGS $opal_Cppflags"
           CPPFLAGS="$CPPFLAGS $opal_Cppflags"
           AC_CHECK_HEADERS([$3], [$5], [$6], [$4])],
          [AS_IF([test -z "$opal_With" || test "$opal_With" = "yes"],
                 [AC_CHECK_HEADERS([$3], [$5], [$6], [$4])],
                 [AS_IF([test -r "$opal_With/$3"],
                        [$2_CPPFLAGS="$$2_CPPFLAGS -I$opal_With"
                         CPPFLAGS="$CPPFLAGS -I$opal_With"
                         AC_CHECK_HEADERS([$3], [$5], [$6], [$4])],
                        [AS_IF([test -r "$opal_With/include/$3"],
                               [$2_CPPFLAGS="$$2_CPPFLAGS -I$opal_With/include"
                                CPPFLAGS="$CPPFLAGS -I$opal_With/include"
                                AC_CHECK_HEADERS([$3], [$5], [$6], [$4])],
                               [$6])])])])

    AS_VAR_POPDEF([opal_Cppflags])
    AS_VAR_POPDEF([opal_With])
])

dnl _OPAL_CHECK_PACKAGE2_LIB(package, prefix, function, library, extra-libraries,
dnl                          [action-if-found], [action-if-not-found]])
dnl --------------------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_PACKAGE2_LIB], [
    # This is stolen from autoconf to peek under the covers to get the
    # cache variable for the library check.  one should not copy this
    # code into other places unless you want much pain and suffering
    AS_VAR_PUSHDEF([opal_Lib], [ac_cv_search_$3])

    # so this sucks, but there's no way to get through the progression
    # of search libs without killing off the cache variable and trying
    # again...
    unset opal_Lib

    AS_VAR_PUSHDEF([opal_With], [with_$1])
    AS_VAR_PUSHDEF([opal_Ldflags], [with_$1_ldflags])

    opal_check_package2_lib_happy="no"
    AS_IF([test -n "$opal_Ldflags" || test -z "$opal_With" || test "$opal_With" = "yes"],
          [# ldflags was specified - use as is
           $2_LDFLAGS="$$2_LDFLAGS $opal_Ldflags"
           LDFLAGS="$LDFLAGS $opal_Ldflags"
           AS_IF([test -z "$opal_Ldflags"],
                 [AC_VERBOSE([looking for library without search path])])
           AC_SEARCH_LIBS([$3], [$4],
                        [opal_check_package2_lib_happy="yes"],
                        [opal_check_package2_lib_happy="no"], [$5])
           AS_IF([test "$opal_check_package2_lib_happy" = "no"],
                 [LDFLAGS="$opal_check_package_$2_save_LDFLAGS"
                  $2_LDFLAGS="$opal_check_package_$2_orig_LDFLAGS"
                  unset opal_Lib])],
          [AS_IF([test -d "$opal_With/lib"],
                 [$2_LDFLAGS="$$2_LDFLAGS -L$opal_With/lib"
                  LDFLAGS="$LDFLAGS -L$opal_With/lib"
                  AC_VERBOSE([looking for library in lib])
                  AC_SEARCH_LIBS([$3], [$4],
                                 [opal_check_package2_lib_happy="yes"],
                                 [opal_check_package2_lib_happy="no"], [$5])
                  AS_IF([test "$opal_check_package2_lib_happy" = "no"],
                        [# no go on the as is..  see what happens later...
                         LDFLAGS="$opal_check_package_$2_save_LDFLAGS"
                         $2_LDFLAGS="$opal_check_package_$2_orig_LDFLAGS"
                         unset opal_Lib])])

           AS_IF([test "$opal_check_package2_lib_happy" = "no" && test -d "$opal_With/lib64"],
                 [$2_LDFLAGS="$$2_LDFLAGS -L$opal_With/lib64"
                  LDFLAGS="$LDFLAGS -L$opal_With/lib64"
                  AC_VERBOSE([looking for library in lib64])
                  AC_SEARCH_LIBS([$3], [$4],
                                 [opal_check_package2_lib_happy="yes"],
                                 [opal_check_package2_lib_happy="no"], [$5])
                  AS_IF([test "$opal_check_package2_lib_happy" = "no"],
                        [# no go on the as is..  see what happens later...
                         LDFLAGS="$opal_check_package_$2_save_LDFLAGS"
                         $2_LDFLAGS="$opal_check_package_$2_orig_LDFLAGS"
                         unset opal_Lib])])])

    AS_IF([test "$opal_check_package2_lib_happy" = "yes"],
          [ # libnl v1 and libnl3 are known to *not* coexist
            # harmoniously in the same process.  Check to see if this
            # new package will introduce such a conflict.
           OPAL_LIBNL_SANITY_CHECK([$4], [$3], [$$2_LIBS],
                                   [opal_check_package_libnl_check_ok])
           AS_IF([test $opal_check_package_libnl_check_ok -eq 0],
                 [opal_check_package2_lib_happy=no])
           ])

    AS_IF([test "$opal_check_package2_lib_happy" = "yes"],
          [ # The result of AC SEARCH_LIBS is cached in $ac_cv_search_[function]
           AS_IF([test "$ac_cv_search_$3" != "no" &&
                  test "$ac_cv_search_$3" != "none required"],
                 [$2_LIBS="$ac_cv_search_$3 $5"],
                 [$2_LIBS="$5"])
           $6],
          [$7])

    AS_VAR_POPDEF([opal_Ldflags])dnl
    AS_VAR_POPDEF([opal_With])dnl
    AS_VAR_POPDEF([opal_Lib])dnl
])

dnl OPAL_CHECK_PACKAGE(package,
dnl                    prefix,
dnl                    header,
dnl                    includes,
dnl                    function,
dnl                    library,
dnl                    extra-libraries,
dnl                    [action-if-found],
dnl                    [action-if-not-found])
dnl -----------------------------------------------------------
dnl Check for package defined by header and libs, and probably
dnl located in dir-prefix, possibly with libs in libdir-prefix.
dnl Both dir-prefix and libdir-prefix can be empty.  Will set
dnl prefix_{CPPFLAGS, LDFLAGS, LIBS} as needed.
dnl
dnl The general intent of this macro is to provide finer-grained scoping
dnl of C preprocessor flags, linker flags, and libraries (as opposed to
dnl unconditionally adding to the top-level CPFLAGS, LDFLAGS, and LIBS,
dnl which get used to compile/link *everything*).
dnl
dnl Here is a breakdown of the parameters:
dnl
dnl * package: the macro checks $with_$package, $with_$package_cppflags
dnl   and $with_$package_ldflags
dnl * prefix: the macro sets $prefix_CPPFLAGS, $prefix_LDFLAGS, and
dnl   $prefix_LIBS (and AC_SUBSTs all of them).  For example, if a
dnl   provider uses this macro to check for a header/library that it
dnl   needs, it might well set prefix to be its provider name.
dnl * header: the foo.h file to check for
dnl * includes: if including header_filename requires additional
dnl   headers to be included first, list them here
dnl * function/library: check for function $function in
dnl   -l$library.  Specifically, for $library, use the "foo" form,
dnl   as opposed to "libfoo".
dnl * extra_libraries: if the $library you are checking for requires
dnl   additonal -l arguments to link successfully, list them here.
dnl * action_if_found: if both the header and library are found and
dnl   usable, execute action_if_found
dnl * action_if_not_found: otherwise, execute action_if_not_found
dnl
dnl The output _CPPFLAGS, _LDFLAGS, and _LIBS can be used to limit the
dnl scope various flags in Makefiles.
dnl
AC_DEFUN([OPAL_CHECK_PACKAGE2],[
    OPAL_VAR_SCOPE_PUSH([opal_check_package2_happy opal_check_package_$2_save_CPPFLAGS opal_check_package_$2_save_LDFLAGS opal_check_package_$2_save_LIBS opal_check_package_$2_orig_CPPFLAGS opal_check_package_$2_orig_LDFLAGS opal_check_package_$2_orig_LIBS opal_check_package2_lib_happy])
    opal_check_package_$2_save_CPPFLAGS="$CPPFLAGS"
    opal_check_package_$2_save_LDFLAGS="$LDFLAGS"
    opal_check_package_$2_save_LIBS="$LIBS"

    opal_check_package_$2_orig_CPPFLAGS="$$2_CPPFLAGS"
    opal_check_package_$2_orig_LDFLAGS="$$2_LDFLAGS"
    opal_check_package_$2_orig_LIBS="$$2_LIBS"

    AS_VAR_PUSHDEF([opal_With], [with_$1])

    AS_IF([test "$opal_With" != "no"],
          [_OPAL_CHECK_PACKAGE2_HEADER([$1], [$2], [$3], [$4],
                                       [_OPAL_CHECK_PACKAGE2_LIB([$1], [$2], [$5], [$6], [$7],
                                                                 [opal_check_package2_happy="yes"],
                                                                 [opal_check_package2_happy="no"])])])

    AS_IF([test "$opal_check_package2_happy" = "yes"],
          [$8],
          [$2_CPPFLAGS="$opal_check_package_$2_orig_CPPFLAGS"
           $2_LDFLAGS="$opal_check_package_$2_orig_LDFLAGS"
           $2_LIBS="$opal_check_package_$2_orig_LIBS"
           $9])


    CPPFLAGS="$opal_check_package_$2_save_CPPFLAGS"
    LDFLAGS="$opal_check_package_$2_save_LDFLAGS"
    LIBS="$opal_check_package_$2_save_LIBS"

    AS_VAR_POPDEF([opal_With])dnl
    OPAL_VAR_SCOPE_POP
])
