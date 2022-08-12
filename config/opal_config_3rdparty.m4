dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2009-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2020-2021 Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl OPAL_3RDPARTY_WITH(short package name, long package name,
dnl                    internal supported, disabled ok)
dnl
dnl Basic --with-pkg/--with-pkg-libdir handling for 3rd party
dnl packages, with the big long description of internal/external/path
dnl handling.
dnl
dnl At the end of this macro, with_pkg will contain an empty string or
dnl a path (the later implying external).  Further, the shell variable
dnl opal_pkg_mode will be set to "internal", "external",
dnl "unspecified", or "disabled".  If a path is given to --with-pkg, then
dnl opal_pkg_mode will be set to external.  If "internal supported" is
dnl not defined, then opal_pkg_mode will not be internal.  If
dnl "disabled ok" is not defined, then opal_pkg_mode will not be
dnl "disabled".
dnl
dnl If m4_ifdef(internal support) does not evaluate to true (ie, at
dnl autogen time), the references to internal in the help strings will
dnl be removed and internal will not be a supported option.
dnl
dnl If m4_ifval(ddisbaled ok) does not evaluate to true (ie, at autogen
dnl time), then --without-pkg will not be a valid configure option and
dnl will raise an error.
dnl
dnl $1: short package name
dnl $2: long pacakage name
AC_DEFUN([OPAL_3RDPARTY_WITH], [
    m4_ifval([$4],
        [m4_ifdef([$3],
            [AC_ARG_WITH([$1],
                [AS_HELP_STRING([--with-$1(=DIR)],
                                [Build $2 support.  DIR can take one of four values: "internal", "external", "no", or a valid directory name.  "internal" forces Open MPI to use its internal copy of $2.  "external" forces Open MPI to use an external installation of $2.  Supplying a valid directory name also forces Open MPI to use an external installation of $2, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries. "no" means that Open MPI will not build components that require this package.  If no argument is specified, Open MPI will search default locations for $2 and fall back to an internal version if one is not found.])])],
            [AC_ARG_WITH([$1],
                [AS_HELP_STRING([--with-$1(=DIR)],
                                [Build $2 support.  DIR can take one of three values: "external", "no", or a valid directory name.  "external" forces Open MPI to use an external installation of $2.  Supplying a valid directory name also forces Open MPI to use an external installation of $2, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries. "no" means that Open MPI will not build components that require this package.  If no argument is specified, Open MPI will search default locations for $2 and error if one is not found.])])])],
        [m4_ifdef([$3],
            [AC_ARG_WITH([$1],
                [AS_HELP_STRING([--with-$1(=DIR)],
                                [Build $2 support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" forces Open MPI to use its internal copy of $2.  "external" forces Open MPI to use an external installation of $2.  Supplying a valid directory name also forces Open MPI to use an external installation of $2, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries. Note that Open MPI no longer supports --without-$1.  If no argument is specified, Open MPI will search default locations for $2 and fall back to an internal version if one is not found.])])],
            [AC_ARG_WITH([$1],
                [AS_HELP_STRING([--with-$1(=DIR)],
                                [Build $2 support.  DIR can take one of two values: "external" or a valid directory name.  "external" forces Open MPI to use an external installation of $2.  Supplying a valid directory name also forces Open MPI to use an external installation of $2, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries. Note that Open MPI no longer supports --without-$1.  If no argument is specified, Open MPI will search default locations for $2 and error if one is not found.])])])])

    AC_ARG_WITH([$1-libdir],
       [AS_HELP_STRING([--with-$1-libdir=DIR],
           [Search for $2 libraries in DIR.  Should only be used if an external copy of $2 is being used.])])

    # Bozo check
    m4_ifval([$4], [],
        [AS_IF([test "$with_$1" = "no"],
               [AC_MSG_WARN([It is not possible to configure Open MPI --without-$1])
                AC_MSG_ERROR([Cannot continue])])])

    AS_IF([test "$with_$1_libdir" = "no" -o "$with_$1_libdir" = "yes"],
          [AC_MSG_WARN([yes/no are invalid responses for --with-$1-libdir.  Please specify a path.])
           AC_MSG_ERROR([Cannot continue])])

    # Make sure the user didn't specify --with-$1=internal and
    # --with-$1-libdir=whatever (because you can only specify
    # --with-$1-libdir when external $2 is being used).
    AS_IF([test "$with_$1" = "internal" && test -n "$with_$1_libdir"],
          [AC_MSG_WARN([Both --with-$1=internal and --with-$1-libdir=DIR])
           AC_MSG_WARN([were specified, which does not make sense.])
           AC_MSG_ERROR([Cannot continue])])

    # clean up $with_$1 so that it contains only a path or empty
    # string.  To determine internal or external preferences, use
    # $opal_$1_mode.
    AS_IF([test "$with_$1" = "yes"], [with_$1=])
    AS_CASE([$with_$1],
            ["internal"], [with_$1=""
                           opal_$1_mode="internal"],
            ["external"], [with_$1=""
                           opal_$1_mode="external"],
            ["no"],       [with_$1=""
                           opal_$1_mode="disabled"],
            [""], [opal_$1_mode="unspecified"],
            [opal_$1_mode="external"])

    m4_ifdef([$3], [],
             [AS_IF([test "$opal_$1_mode" = "internal"],
                    [AC_MSG_WARN([Invalid argument to --with-$1: internal.])
                     AC_MSG_ERROR([Cannot continue])])])
])
