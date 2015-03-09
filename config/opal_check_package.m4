dnl -*- shell-script -*-
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
dnl Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl _OPAL_CHECK_PACKAGE_HEADER(prefix, header, dir-prefix,
dnl                            [action-if-found], [action-if-not-found],
dnl                            includes)
dnl --------------------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_PACKAGE_HEADER], [
    # This is stolen from autoconf to peek under the covers to get the
    # cache variable for the library check.  one should not copy this
    # code into other places unless you want much pain and suffering
    AS_VAR_PUSHDEF([opal_Header], [ac_cv_header_$2])

    # so this sucks, but there's no way to get through the progression
    # of header includes without killing off the cache variable and trying
    # again...
    unset opal_Header

    opal_check_package_header_happy="no"
    AS_IF([test "$3" = "/usr" || \
           test "$3" = "/usr/local"],
           [ # try as is...
            AC_VERBOSE([looking for header without includes])
            AC_CHECK_HEADERS([$2], [opal_check_package_header_happy="yes"], [])
            AS_IF([test "$opal_check_package_header_happy" = "no"],
                  [# no go on the as is - reset the cache and try again
                   unset opal_Header])])

    AS_IF([test "$opal_check_package_header_happy" = "no"],
          [AS_IF([test "$3" != ""],
                 [$1_CPPFLAGS="$$1_CPPFLAGS -I$3/include"
                  CPPFLAGS="$CPPFLAGS -I$3/include"])
          AC_CHECK_HEADERS([$2], [opal_check_package_header_happy="yes"], [], [$6])
	  AS_IF([test "$opal_check_package_header_happy" = "yes"], [$4], [$5])],
          [$4])
    unset opal_check_package_header_happy

    AS_VAR_POPDEF([opal_Header])dnl
])


dnl _OPAL_CHECK_PACKAGE_LIB(prefix, library, function, extra-libraries,
dnl                         dir-prefix, libdir,
dnl                         [action-if-found], [action-if-not-found]])
dnl --------------------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_PACKAGE_LIB], [
    # This is stolen from autoconf to peek under the covers to get the
    # cache variable for the library check.  one should not copy this
    # code into other places unless you want much pain and suffering
    AS_LITERAL_IF([$2],
                  [AS_VAR_PUSHDEF([opal_Lib], [ac_cv_search_$2_$3])],
                  [AS_VAR_PUSHDEF([opal_Lib], [ac_cv_search_$2''_$3])])dnl

    # see comment above
    unset opal_Lib
    opal_check_package_lib_happy="no"
    AS_IF([test "$6" != ""],
          [ # libdir was specified - search only there
           $1_LDFLAGS="$$1_LDFLAGS -L$6"
           LDFLAGS="$LDFLAGS -L$6"
           AC_SEARCH_LIBS([$3], [$2],
                        [opal_check_package_lib_happy="yes"],
                        [opal_check_package_lib_happy="no"], [$4])
           AS_IF([test "$opal_check_package_lib_happy" = "no"],
                 [LDFLAGS="$opal_check_package_$1_save_LDFLAGS"
                  $1_LDFLAGS="$opal_check_package_$1_orig_LDFLAGS"
                  unset opal_Lib])],
          [ # libdir was not specified - go through search path
           opal_check_package_libdir="$5"
           AS_IF([test "$opal_check_package_libdir" = "" || \
                  test "$opal_check_package_libdir" = "/usr" || \
                  test "$opal_check_package_libdir" = "/usr/local"],
               [ # try as is...
                AC_VERBOSE([looking for library without search path])
                AC_SEARCH_LIBS([$3], [$2],
                        [opal_check_package_lib_happy="yes"],
                        [opal_check_package_lib_happy="no"], [$4])
                AS_IF([test "$opal_check_package_lib_happy" = "no"],
                    [ # no go on the as is..  see what happens later...
                     LDFLAGS="$opal_check_package_$1_save_LDFLAGS"
                     $1_LDFLAGS="$opal_check_package_$1_orig_LDFLAGS"
                     unset opal_Lib])])

           AS_IF([test "$opal_check_package_lib_happy" = "no"],
               [AS_IF([test "$opal_check_package_libdir" != ""],
                    [$1_LDFLAGS="$$1_LDFLAGS -L$opal_check_package_libdir/lib"
                     LDFLAGS="$LDFLAGS -L$opal_check_package_libdir/lib"
                     AC_VERBOSE([looking for library in lib])
                     AC_SEARCH_LIBS([$3], [$2],
                               [opal_check_package_lib_happy="yes"],
                               [opal_check_package_lib_happy="no"], [$4])
                     AS_IF([test "$opal_check_package_lib_happy" = "no"],
                         [ # no go on the as is..  see what happens later...
                          LDFLAGS="$opal_check_package_$1_save_LDFLAGS"
                          $1_LDFLAGS="$opal_check_package_$1_orig_LDFLAGS"
                          unset opal_Lib])])])

           AS_IF([test "$opal_check_package_lib_happy" = "no"],
               [AS_IF([test "$opal_check_package_libdir" != ""],
                    [$1_LDFLAGS="$$1_LDFLAGS -L$opal_check_package_libdir/lib64"
                     LDFLAGS="$LDFLAGS -L$opal_check_package_libdir/lib64"
                     AC_VERBOSE([looking for library in lib64])
                     AC_SEARCH_LIBS([$3], [$2],
                               [opal_check_package_lib_happy="yes"],
                               [opal_check_package_lib_happy="no"], [$4])
                     AS_IF([test "$opal_check_package_lib_happy" = "no"],
                         [ # no go on the as is..  see what happens later...
                          LDFLAGS="$opal_check_package_$1_save_LDFLAGS"
                          $1_LDFLAGS="$opal_check_package_$1_orig_LDFLAGS"
                          unset opal_Lib])])])])

    AS_IF([test "$opal_check_package_lib_happy" = "yes"],
          [ # The result of AC SEARCH_LIBS is cached in $ac_cv_search_[function]
           AS_IF([test "$ac_cv_search_$3" != "no" &&
                  test "$ac_cv_search_$3" != "none required"],
                 [$1_LIBS="$ac_cv_search_$3 $4"],
                 [$1_LIBS="$4"])
           $7],
          [$8])

    AS_VAR_POPDEF([opal_Lib])dnl
])


dnl FI_CHECK_PACKAGE(prefix,
dnl                    header,
dnl                    library,
dnl                    function,
dnl                    extra-libraries,
dnl                    dir-prefix,
dnl                    libdir-prefix,
dnl                    [action-if-found], [action-if-not-found],
dnl                    includes)
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
dnl * prefix: the macro sets $prefix_CPPFLAGS, $prefix_LDFLAGS, and
dnl   $prefix_LIBS (and AC_SUBSTs all of them).  For example, if a
dnl   provider uses this macro to check for a header/library that it
dnl   needs, it might well set prefix to be its provider name.
dnl * header_filename: the foo.h file to check for
dnl * library_name / function_name: check for function function_name in
dnl   -llibrary_name.  Specifically, for library_name, use the "foo" form,
dnl   as opposed to "libfoo".
dnl * extra_libraries: if the library_name you are checking for requires
dnl   additonal -l arguments to link successfully, list them here.
dnl * dir_prefix: if the header/library is located in a non-standard
dnl   location (e.g., /opt/foo as opposed to /usr), list it here
dnl * libdir_prefix: if the library is not under $dir_prefix/lib or
dnl   $dir_prefix/lib64, list it here.
dnl * action_if_found: if both the header and library are found and
dnl   usable, execute action_if_found
dnl * action_if_not_found: otherwise, execute action_if_not_found
dnl * extra_includes: if including header_filename requires additional
dnl   headers to be included first, list them here
dnl
dnl The output _CPPFLAGS, _LDFLAGS, and _LIBS can be used to limit the
dnl scope various flags in Makefiles.
dnl
AC_DEFUN([OPAL_CHECK_PACKAGE],[
    opal_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    opal_check_package_$1_save_LDFLAGS="$LDFLAGS"
    opal_check_package_$1_save_LIBS="$LIBS"

    opal_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    opal_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    opal_check_package_$1_orig_LIBS="$$1_LIBS"

    _OPAL_CHECK_PACKAGE_HEADER([$1], [$2], [$6],
          [_OPAL_CHECK_PACKAGE_LIB([$1], [$3], [$4], [$5], [$6], [$7],
                [opal_check_package_happy="yes"],
                [opal_check_package_happy="no"])],
          [opal_check_package_happy="no"],
          [$10])

    AS_IF([test "$opal_check_package_happy" = "yes"],
          [$8],
          [$1_CPPFLAGS="$opal_check_package_$1_orig_CPPFLAGS"
           $1_LDFLAGS="$opal_check_package_$1_orig_LDFLAGS"
           $1_LIBS="$opal_check_package_$1_orig_LIBS"
           $9])

    CPPFLAGS="$opal_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$opal_check_package_$1_save_LDFLAGS"
    LIBS="$opal_check_package_$1_save_LIBS"
])
