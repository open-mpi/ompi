dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2017      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl
dnl More libnl v1/v3 sadness: the two versions are not compatible
dnl and will not work correctly if simultaneously linked into the
dnl same applications.  Unfortunately, they *will* link into the
dnl same image!  On platforms like CentOS 7, libibverbs depends on
dnl libnl-3.so.200 and friends, so if libnl3-devel packages are not
dnl installed, but libnl-devel are, Open MPI should not try to use libnl.
dnl
dnl GROSS: libnl wants us to either use pkg-config (which we
dnl cannot assume is always present) or we need to look in a
dnl particular directory for the right libnl3 include files.  For
dnl now, just hard code the special path into this logic.
dnl
dnl OAC_CHEC_PACKAGE() invokes OPAL_LIBNL_CHECK_PACKAGE_CALLBACK() in order
dnl to keep track of which libraries depend on libnl and which libraries
dnl depend on libnl3.
dnl Open MPI will not be able to build a component vs a given version of libnl
dnl if the other libnl version is required by some third party components.
dnl At the end of configure, display a summary of who is using what, and aborts
dnl if both libnl versions are required.

dnl OPAL_LIBNL_SANITY_INIT()
dnl --------------------------------------------------------------------
AC_DEFUN([OPAL_LIBNL_SANITY_INIT], [
    opal_libnl_version=0
    opal_libnlv1_libs=
    opal_libnlv3_libs=

    AC_ARG_WITH([libnl],
                [AS_HELP_STRING([--with-libnl(=DIR)],
                                [Directory prefix for libnlv3 (typically only necessary if libnl is installed in a location that the compiler/linker will not search by default)])])
    AC_ARG_WITH([libn-libdirl],
                [AS_HELP_STRING([--with-libnl-libdir=DIR],
                                [Directory prefix for libnlv3 libs(typically only necessary if libnl is installed in a location that the compiler/linker will not search by default)])])

    # ugly hack to deal with potentially alternate locations for
    # libnl3 headers.  Note that if the pkg-config file is found,
    # this ugly hack won't be used.
    AS_IF([test -n "$with_libnl_incdir"],
          [# skip check if someone above set incdir],
          [test -d "/usr/include/libnl3"],
          [with_libnl_incdir="/usr/include/libnl3"],
          [test -d "/usr/local/include/libnl3"],
          [with_libnl_incdir="/usr/local/include/libnl3"])

    OAC_CHECK_PACKAGE_VERIFY_COMMANDS([[OPAL_LIBNL_CHECK_PACKAGE_CALLBACK]])
])

dnl OPAL_LIBNL_SANITY_FAIL_MSG(lib)
dnl
dnl Helper to pring a big warning message when we detect a libnl conflict.
dnl
dnl --------------------------------------------------------------------
AC_DEFUN([OPAL_LIBNL_SANITY_FAIL_MSG], [
    AC_MSG_WARN([This is a configuration that is *known* to cause run-time crashes.])
    AC_MSG_WARN([This is an error in lib$1 (not Open MPI).])
    AC_MSG_WARN([Open MPI will therefore skip using lib$1.])
])

dnl OPAL_LIBNL_CHECK_PACKAGE_CALLBACK(package name, prefix,
dnl                   headers, function name,
dnl                   action if happy, action if not happy)
dnl
dnl Callback from OAC_CHECK_PACKAGE to verify that there is
dnl not a conflict.  Note that CPPFLAGS, LDFLAGS, and LIBS
dnl are setup to compile/link package.
AC_DEFUN([OPAL_LIBNL_CHECK_PACKAGE_CALLBACK], [
    OPAL_VAR_SCOPE_PUSH([opal_libnl_sane])
    opal_libnl_sane=1
    case $host in
        *linux*)
            OPAL_LIBNL_SANITY_CHECK_LINUX([$1], [$4], [], [opal_libnl_sane])
            ;;
    esac

    AS_IF([test ${opal_libnl_sane} -eq 1],
          [$5], [$6])

    OPAL_VAR_SCOPE_POP([opal_libnl_sane])
])

dnl
dnl Simple helper for OPAL_LIBNL_SANITY_CHECK
dnl $1: package name
dnl $2: function
dnl $3: LIBS
dnl $4: output variable (1=ok, 0=not ok)
dnl
AC_DEFUN([OPAL_LIBNL_SANITY_CHECK_LINUX], [
    OPAL_VAR_SCOPE_PUSH([this_requires_v1 libnl_sane this_requires_v3 ldd_output result_msg])

    AS_VAR_PUSHDEF([libnl_check_lib], [opal_libnl_sanity_check_cv_$1])

    AC_CACHE_CHECK([if $1 requires libnl v1 or v3],
        [libnl_check_lib],
        [AC_LANG_PUSH([C])
         cat > conftest_c.$ac_ext << EOF
extern void $2 (void);
int main(int argc, char *argv[[]]) {
    $2 ();
    return 0;
}
EOF

         result_msg=
         OPAL_LOG_COMMAND([$CC -o conftest $CFLAGS $CPPFLAGS conftest_c.$ac_ext $LDFLAGS $LIBS $3],
             [ldd_output=`ldd conftest`
              AS_IF([echo $ldd_output | grep -q libnl-3.so],
                    [result_msg="v3"])
              AS_IF([echo $ldd_output | grep -q libnl.so],
                    [OPAL_APPEND([result_msg], ["v1"])])
              AS_IF([test -z "${result_msg}"], [result_msg="none"])],
             [AC_MSG_WARN([Could not link a simple program with lib $1])])
         AC_LANG_POP([C])
         AS_VAR_SET([libnl_check_lib], [${result_msg}])
         rm -f conftest conftest_c.$ac_ext])
    AS_VAR_COPY([result_msg], [libnl_check_lib])
    this_requires_v1=0
    this_requires_v3=0
    AS_IF([echo "${result_msg}" | grep -q v1], [this_requires_v1=1])
    AS_IF([echo "${result_msg}" | grep -q v3], [this_requires_v3=1])

    AS_VAR_POPDEF([libnl_check_lib])

    # Assume that our configuration is sane; this may get reset below
    libnl_sane=1

    # Note: in all the checks below, only add this library to the list
    # of libraries (for v1 or v3 as relevant) if we do not fail.
    # I.e., assume that a higher level will refuse to use this library
    # if we return failure.

    # Does this library require both v1 and v3?  If so, fail.
    AS_IF([test $this_requires_v1 -eq 1 && test $this_requires_v3 -eq 1],
          [AC_MSG_WARN([Unfortunately, $1 links to both libnl and libnl-3.])
           OPAL_LIBNL_SANITY_FAIL_MSG($1)
           libnl_sane=0])

    # Does this library require v1, but some prior library required
    # v3?  If so, fail.
    AS_IF([test $libnl_sane -eq 1 && test $this_requires_v1 -eq 1],
          [AS_IF([test $opal_libnl_version -eq 3],
                 [AC_MSG_WARN([libnl version conflict: $opal_libnlv3_libs requires libnl-3 whereas $1 requires libnl])
                  OPAL_LIBNL_SANITY_FAIL_MSG($1)
                  libnl_sane=0],
                 [opal_libnlv1_libs="$opal_libnlv1_libs $1"
                  OPAL_UNIQ([opal_libnlv1_libs])
                  opal_libnl_version=1])
           ])

    # Does this library require v3, but some prior library required
    # v1?  If so, fail.
    AS_IF([test $libnl_sane -eq 1 && test $this_requires_v3 -eq 1],
          [AS_IF([test $opal_libnl_version -eq 1],
                 [AC_MSG_WARN([libnl version conflict: $opal_libnlv1_libs requires libnl whereas $1 requires libnl-3])
                  OPAL_LIBNL_SANITY_FAIL_MSG($1)
                  libnl_sane=0],
                 [opal_libnlv3_libs="$opal_libnlv3_libs $1"
                  OPAL_UNIQ([opal_libnlv3_libs])
                  opal_libnl_version=3])
          ])

    $4=$libnl_sane

    OPAL_VAR_SCOPE_POP([ldd_output libnl_sane this_requires_v1 this_requires_v3 result_msg])
])

dnl
dnl Summarize libnl and libnl3 usage,
dnl and abort if conflict is found
dnl
dnl Print the list of libraries that use libnl,
dnl the list of libraries that use libnl3,
dnl and aborts if both libnl and libnl3 are used.
dnl
AC_DEFUN([OPAL_CHECK_LIBNL_SUMMARY],[
    AC_MSG_CHECKING([for libraries that use libnl v1])
    AS_IF([test -n "$opal_libnlv1_libs"],
          [AC_MSG_RESULT([$opal_libnlv1_libs])],
          [AC_MSG_RESULT([(none)])])
    AC_MSG_CHECKING([for libraries that use libnl v3])
    AS_IF([test -n "$opal_libnlv3_libs"],
          [AC_MSG_RESULT([$opal_libnlv3_libs])],
          [AC_MSG_RESULT([(none)])])
    AS_IF([test -n "$opal_libnlv1_libs" && test -n "$opal_libnlv3_libs"],
          [AC_MSG_WARN([libnl v1 and libnl v3 have been found as dependent libraries])
           AC_MSG_ERROR([This is a configuration that is known to cause run-time crashes])])
])
