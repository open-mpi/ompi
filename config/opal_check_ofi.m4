dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2023      Triad National Security, LLC. All rights
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

    AS_LITERAL_WORD_IF([$1], [], [m4_fatal([OPAL_CHECK_OFI_VERSION_GE called with non-literal first argument])])dnl
    AS_VAR_PUSHDEF([version_cache_var], [opal_ofi_ver_ge_cv_$1])dnl
    m4_pushdef([version_pretty_print], [m4_translit([$1], [,], [.])])dnl

    AC_CACHE_CHECK([if OFI API version number is >= version_pretty_print],
        [version_cache_var],
        [opal_ofi_ver_ge_save_CPPFLAGS=$CPPFLAGS
         CPPFLAGS=$opal_ofi_internal_CPPFLAGS

         AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
                [[#include <rdma/fabric.h>
]], [[
#if !defined(FI_MAJOR_VERSION)
#error "we cannot check the version -- sad panda"
#elif FI_VERSION_LT(FI_VERSION(FI_MAJOR_VERSION, FI_MINOR_VERSION), FI_VERSION($1))
#error "version is too low -- nopes"
#endif
]])],
                      [version_cache_var=yes],
                      [version_cache_var=no])])

    AS_IF([test "${version_cache_var}" = "yes"],
          [$2],
          [$3])

    CPPFLAGS=$opal_ofi_ver_ge_save_CPPFLAGS

    m4_popdef([version_pretty_print])
    AS_VAR_POPDEF([version_cache_var])
    OPAL_VAR_SCOPE_POP
])dnl


dnl OPAL_CHECK_OFI(prefix, [action if found], [action if not found])
dnl --------------------------------------------------------
dnl Do the real work of checking for OFI libfabric.
dnl Upon return:
dnl
dnl opal_ofi_{CPPFLAGS, LDFLAGS, LIBS} and prefix_{CPPFLAGS, LDFLAGS,
dnl LIBS} will be set as needed.
dnl
dnl This macro intentionally leaks opal_ofi_happy = yes/no as well as
dnl evaluating the action if found / action if not found
dnl
AC_DEFUN([OPAL_CHECK_OFI],[
    OPAL_VAR_SCOPE_PUSH([opal_check_ofi_save_CPPFLAGS opal_check_ofi_save_LDFLAGS opal_check_ofi_save_LIBS opal_check_fi_info_pci])

    m4_ifblank([$1], [m4_fatal([First argument must be set for call to OPAL_CHECK_OFI])])

    # Add --with options
    AC_ARG_WITH([libfabric],
                [AS_HELP_STRING([--with-libfabric=DIR],
                                [Deprecated synonym for --with-ofi])])
    AC_ARG_WITH([libfabric-libdir],
                [AS_HELP_STRING([--with-libfabric-libdir=DIR],
                                [Deprecated synonym for --with-ofi-libdir])])

    AC_ARG_WITH([ofi],
                [AS_HELP_STRING([--with-ofi=DIR],
                                [Specify location of OFI libfabric installation, adding DIR/include to the default search location for libfabric headers, and DIR/lib or DIR/lib64 to the default search location for libfabric libraries.  Error if libfabric support cannot be found.])])
    AC_ARG_WITH([ofi-libdir],
                [AS_HELP_STRING([--with-ofi-libdir=DIR],
                                [Search for OFI libfabric libraries in DIR])])

    AS_IF([test -z "${with_ofi}"], [with_ofi=${with_libfabric}])
    AS_IF([test -z "${with_ofi_libdir}"], [with_ofi_libdir=${with_libfabric_libdir}])

    opal_check_ofi_save_CPPFLAGS=${CPPFLAGS}
    opal_check_ofi_save_LDFLAGS=${LDFLAGS}
    opal_check_ofi_save_LIBS=${LIBS}

    opal_check_fi_info_pci=0

    dnl OMPI has used ofi everywhere for some time, but the pkg-config
    dnl module name is libfabric.  Easier to set the pkg-config module
    dnl name explicitly than change everything in OMPI.
    m4_define([ofi_pkgconfig_module], [libfabric])
    OAC_CHECK_PACKAGE([ofi],
                      [$1],
                      [rdma/fabric.h],
                      [fabric],
                      [fi_getinfo],
                      [opal_ofi_happy=yes],
                      [opal_ofi_happy=no])

    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${$1_CPPFLAGS}])

    AS_IF([test $opal_ofi_happy = yes],
          [AC_CHECK_HEADERS([rdma/fi_ext.h])

           AC_CHECK_MEMBER([struct fi_info.nic],
                           [opal_check_fi_info_pci=1],
                           [opal_check_fi_info_pci=0],
                           [[#include <rdma/fabric.h>]])

           AC_DEFINE_UNQUOTED([OPAL_OFI_PCI_DATA_AVAILABLE],
                              [${opal_check_fi_info_pci}],
                              [check if pci data is available in ofi])

           AC_CHECK_DECLS([FI_OPT_FI_HMEM_P2P],
                          [], [],
                          [#include <rdma/fi_endpoint.h>])

           AC_CHECK_TYPES([struct fi_ops_mem_monitor], [], [],
                          [#ifdef HAVE_RDMA_FI_EXT_H
#include <rdma/fi_ext.h>
#endif
                           ])

           OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${opal_pmix_CPPFLAGS}])
           AC_CHECK_DECLS([PMIX_PACKAGE_RANK],
                          [],
                          [],
                          [#include <pmix.h>])

           AC_CHECK_MEMBER([struct fi_mr_attr.iface],
                           [opal_check_fi_mr_attr_iface=1],
                           [opal_check_fi_mr_attr_iface=0],
                           [[#include <rdma/fi_domain.h>]])

           AC_DEFINE_UNQUOTED([OPAL_OFI_HAVE_FI_MR_IFACE],
                              [${opal_check_fi_mr_attr_iface}],
                              [check if iface avaiable in fi_mr_attr])

           AC_CHECK_DECL([FI_HMEM_ROCR],
                         [opal_check_fi_hmem_rocr=1],
                         [opal_check_fi_hmem_rocr=0],
                         [#include <rdma/fi_domain.h>])

           AC_DEFINE_UNQUOTED([OPAL_OFI_HAVE_FI_HMEM_ROCR],
                              [${opal_check_fi_hmem_rocr}],
                              [check if FI_HMEM_ROCR avaiable in fi_hmem_iface])

           AC_CHECK_DECL([FI_HMEM_ZE],
                         [opal_check_fi_hmem_ze=1],
                         [opal_check_fi_hmem_ze=0],
                         [#include <rdma/fi_domain.h>])

           AC_DEFINE_UNQUOTED([OPAL_OFI_HAVE_FI_HMEM_ZE],
                              [${opal_check_fi_hmem_ze}],
                              [check if FI_HMEM_ZE avaiable in fi_hmem_iface])])

    CPPFLAGS=${opal_check_ofi_save_CPPFLAGS}
    LDFLAGS=${opal_check_ofi_save_LDFLAGS}
    LIBS=${opal_check_ofi_save_LIBS}

    dnl for version compare tests
    opal_ofi_internal_CPPFLAGS="${$1_CPPFLAGS}"

    OPAL_SUMMARY_ADD([Transports], [OpenFabrics OFI Libfabric], [], [${$1_SUMMARY}])

    AS_IF([test "${opal_ofi_happy}" = "yes"],
          [$2],
          [AS_IF([test -n "${with_ofi}" && test "${with_ofi}" != "no"],
                 [AC_MSG_WARN([OFI libfabric support requested (via --with-ofi or --with-libfabric), but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $3])

    OPAL_VAR_SCOPE_POP
])dnl
