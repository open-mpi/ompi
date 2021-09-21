dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2021      Amazon.com, Inc. or its affiliates. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl
dnl OPAL_CHECK_OFI_VERSION_GE
dnl
dnl Check that the OFI API version number is >= a specific value.
dnl
dnl $1: version number to compare, in the form of "major,minor"
dnl     (without quotes) -- i.e., a single token representing the
dnl     arguments to FI_VERSION()
dnl $2: action if OFI API version is >= $1
dnl $3: action if OFI API version is < $1
AC_DEFUN([OPAL_CHECK_OFI_VERSION_GE],[
    OPAL_VAR_SCOPE_PUSH([opal_ofi_ver_ge_save_CPPFLAGS opal_ofi_ver_ge_happy])

    AC_MSG_CHECKING([if OFI API version number is >= $1])
    opal_ofi_ver_ge_save_CPPFLAGS=$CPPFLAGS
    CPPFLAGS=$opal_ofi_CPPFLAGS

    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <rdma/fabric.h>]],
[[
#if !defined(FI_MAJOR_VERSION)
#error "we cannot check the version -- sad panda"
#elif FI_VERSION_LT(FI_VERSION(FI_MAJOR_VERSION, FI_MINOR_VERSION), FI_VERSION($1))
#error "version is too low -- nopes"
#endif
]])],
                      [opal_ofi_ver_ge_happy=1],
                      [opal_ofi_ver_ge_happy=0])

    AS_IF([test $opal_ofi_ver_ge_happy -eq 1],
          [AC_MSG_RESULT([yes])
           $2],
          [AC_MSG_RESULT([no])
           $3])

    CPPFLAGS=$opal_ofi_ver_ge_save_CPPFLAGS

    OPAL_VAR_SCOPE_POP
])dnl

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

    OPAL_VAR_SCOPE_PUSH([opal_check_ofi_save_CPPFLAGS opal_check_ofi_save_LDFLAGS opal_check_ofi_save_LIBS opal_check_fi_info_pci])
    opal_check_ofi_save_CPPFLAGS=$CPPFLAGS
    opal_check_ofi_save_LDFLAGS=$LDFLAGS
    opal_check_ofi_save_LIBS=$LIBS
    opal_check_fi_info_pci=0

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

    CPPFLAGS="$CPPFLAGS $opal_ofi_CPPFLAGS"

    AS_IF([test $opal_ofi_happy = yes],
          [AC_CHECK_HEADERS([rdma/fi_ext.h])

           AC_CHECK_MEMBER([struct fi_info.nic],
                           [opal_check_fi_info_pci=1],
                           [opal_check_fi_info_pci=0],
                           [[#include <rdma/fabric.h>]])

           AC_DEFINE_UNQUOTED([OPAL_OFI_PCI_DATA_AVAILABLE],
                              [$opal_check_fi_info_pci],
                              [check if pci data is available in ofi])

           AC_CHECK_DECLS([PMIX_PACKAGE_RANK],
                          [],
                          [],
                          [#include <pmix.h>])

           AC_CHECK_TYPES([struct fi_ops_mem_monitor], [], [],
                          [#ifdef HAVE_RDMA_FI_EXT_H
#include <rdma/fi_ext.h>
#endif])])

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
