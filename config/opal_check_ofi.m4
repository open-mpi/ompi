dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2015-2019 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl
dnl _OPAL_CHECK_OFI
dnl --------------------------------------------------------
dnl Do the real work of checking for OFI libfabric.
dnl Upon return:
dnl
dnl - opal_ofi_happy: will be "yes" or "no"
dnl - opal_ofi_{CPPFLAGS|LDFLAGS|LIBS} will be loaded (if relevant)
dnl
AC_DEFUN([_OPAL_CHECK_OFI],[
    # Add --with options
    AC_ARG_WITH([libfabric],
                [AC_HELP_STRING([--with-libfabric=DIR],
                                [Deprecated synonym for --with-ofi])])
    AC_ARG_WITH([libfabric-libdir],
                [AC_HELP_STRING([--with-libfabric-libdir=DIR],
                                [Deprecated synonym for --with-ofi-libdir])])

    AC_ARG_WITH([ofi],
                [AC_HELP_STRING([--with-ofi=DIR],
                                [Specify location of OFI libfabric installation, adding DIR/include to the default search location for libfabric headers, and DIR/lib or DIR/lib64 to the default search location for libfabric libraries.  Error if libfabric support cannot be found.])])

    AC_ARG_WITH([ofi-libdir],
                [AC_HELP_STRING([--with-ofi-libdir=DIR],
                                [Search for OFI libfabric libraries in DIR])])

    if test "$with_ofi" = ""; then
         with_ofi=$with_libfabric
    fi

    if test "$with_ofi_libdir" = ""; then
         with_ofi_libdir=$with_libfabric_libdir
    fi

    # Sanity check the --with values
    OPAL_CHECK_WITHDIR([ofi], [$with_ofi],
                       [include/rdma/fabric.h])
    OPAL_CHECK_WITHDIR([ofi-libdir], [$with_ofi_libdir],
                       [libfabric.*])

    OPAL_VAR_SCOPE_PUSH([opal_check_ofi_save_CPPFLAGS opal_check_ofi_save_LDFLAGS opal_check_ofi_save_LIBS])
    opal_check_ofi_save_CPPFLAGS=$CPPFLAGS
    opal_check_ofi_save_LDFLAGS=$LDFLAGS
    opal_check_ofi_save_LIBS=$LIBS

    opal_ofi_happy=yes
    AS_IF([test "$with_ofi" = "no"],
          [opal_ofi_happy=no])

    AS_IF([test $opal_ofi_happy = yes],
          [AC_MSG_CHECKING([looking for OFI libfabric in])
           AS_IF([test "$with_ofi" != "yes"],
                 [opal_ofi_dir=$with_ofi
                  AC_MSG_RESULT([($opal_ofi_dir)])],
                 [AC_MSG_RESULT([(default search paths)])])
           AS_IF([test ! -z "$with_ofi_libdir" && \
                         test "$with_ofi_libdir" != "yes"],
                 [opal_ofi_libdir=$with_ofi_libdir])
          ])

    AS_IF([test $opal_ofi_happy = yes],
          [OPAL_CHECK_PACKAGE([opal_ofi],
                              [rdma/fabric.h],
                              [fabric],
                              [fi_getinfo],
                              [],
                              [$opal_ofi_dir],
                              [$opal_ofi_libdir],
                              [],
                              [opal_ofi_happy=no])])

    CPPFLAGS=$opal_check_ofi_save_CPPFLAGS
    LDFLAGS=$opal_check_ofi_save_LDFLAGS
    LIBS=$opal_check_ofi_save_LIBS

    AC_SUBST([opal_ofi_CPPFLAGS])
    AC_SUBST([opal_ofi_LDFLAGS])
    AC_SUBST([opal_ofi_LIBS])

    OPAL_SUMMARY_ADD([[Transports]],[[OpenFabrics OFI Libfabric]],[],[$opal_ofi_happy])

    OPAL_VAR_SCOPE_POP

    AS_IF([test $opal_ofi_happy = no],
          [AS_IF([test -n "$with_ofi" && test "$with_ofi" != "no"],
                 [AC_MSG_WARN([OFI libfabric support requested (via --with-ofi or --with-libfabric), but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           ])
])dnl


dnl
dnl OPAL_CHECK_OFI
dnl --------------------------------------------------------
dnl Check to see if OFI libfabric is available.
dnl
dnl This is a simple wrapper around _OPAL_CHECK_OFI that just
dnl ensures to only run the checks once.  We do not use AC_REQUIRE
dnl because that re-orders the texts and makes ordering in stdout
dnl quite confusing / difficult to grok.
dnl
AC_DEFUN([OPAL_CHECK_OFI],[
    # Check for OFI libfabric.  Note that $opal_ofi_happy is used in
    # other configure.m4's to know if OFI/libfabric configured
    # successfully.  We only need to run the back-end checks once, but
    # at least emit a "checking..." statement each subsequent time
    # this macro is invoked so that configure's stdout has
    # sensible/logical output.
    AS_IF([test -z "$opal_ofi_happy"],
        [_OPAL_CHECK_OFI],
        [AC_MSG_CHECKING([if OFI libfabric is available])
         AC_MSG_RESULT([$opal_ofi_happy])])
])
