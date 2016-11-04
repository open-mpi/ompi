dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2015-2016 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
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
dnl can't assume is always present) or we need to look in a
dnl particular directory for the right libnl3 include files.  For
dnl now, just hard code the special path into this logic.
dnl
dnl _OPAL_CHECK_PACKAGE_LIB() invokes OPAL_LIBNL_SANITY_CHECK() in order
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
                [AC_HELP_STRING([--with-libnl(=DIR)],
                                [Directory prefix for libnl (typically only necessary if libnl is installed in a location that the compiler/linker will not search by default)])])

    # The --with options carry two pieces of information: 1) do
    # you want a specific version of libnl, and 2) where that
    # version of libnl lives.  For simplicity, let's separate
    # those two pieces of information.
    case "$with_libnl" in
        no)
            # Nope, don't want it
            opal_want_libnl=no
            ;;
        "")
            # Just try to build with libnl
            opal_want_libnl=try
            opal_libnl_location=
            ;;
        yes)
            # Yes, definitely want it
            opal_want_libnl=yes
            opal_libnl_location=
            ;;
        *)
            # Yes, definitely want it -- at a specific location
            opal_want_libnl=yes
            opal_libnl_location=$with_libnl
            ;;
    esac
])

dnl OPAL_LIBNL_SANITY_CHECK(lib, function, LIBS)
dnl --------------------------------------------------------------------
AC_DEFUN([OPAL_LIBNL_SANITY_CHECK], [
    case $host in
        *linux*)
            OPAL_VAR_SCOPE_PUSH([ldd_output libnl_version])
            AC_LANG_PUSH(C)
            cat > conftest_c.$ac_ext << EOF
extern void $2 (void);
int main(int argc, char *argv[[]]) {
    $2 ();
    return 0;
}
EOF
            OPAL_LOG_COMMAND([$CC -o conftest $CFLAGS $CPPFLAGS conftest_c.$ac_ext $LDFLAGS -l$1 $LIBS $3],
                             [ldd_output=`ldd conftest`
                              libnl_version=0
                              AS_IF([echo $ldd_output | grep -q libnl.so],
                                    [AS_IF([test $opal_libnl_version -eq 3],
                                           [AC_MSG_WARN([lib nl version conflict: $opal_libnlv3_libs requires libnl-3 whereas $1 requires libnl])],
                                           [opal_libnlv1_libs="$opal_libnlv1_libs $1"
                                            OPAL_UNIQ([opal_libnlv1_libs])
                                            opal_libnl_version=1])
                                     libnl_version=1])
                              AS_IF([echo $ldd_output | grep -q libnl-3.so],
                                    [AS_IF([test $libnl_version -eq 1],
                                           [AC_MSG_WARN([lib $1 requires both libnl v1 and libnl v3 -- yoinks!])
                                            AC_MSG_WARN([This is a configuration that is known to cause run-time crashes])
                                            AC_MSG_ERROR([Cannot continue])])
                                     AS_IF([test $opal_libnl_version -eq 1],
                                           [AC_MSG_WARN([lib nl version conflict: $opal_libnlv1_libs requires libnl whereas $1 requires libnl-3])],
                                           [opal_libnlv3_libs="$opal_libnlv3_libs $1"
                                            OPAL_UNIQ([opal_libnlv3_libs])
                                            opal_libnl_version=3])])
                              rm -f conftest conftest_c.$ac_ext],
                             [AC_MSG_WARN([Could not link a simple program with lib $1])])
            AC_LANG_POP(C)
            OPAL_VAR_SCOPE_POP([ldd_output libnl_version])
            ;;
        esac
])

dnl
dnl Check for libnl-3.
dnl
dnl Inputs:
dnl
dnl $1: prefix where to look for libnl-3
dnl $2: var name prefix of _CPPFLAGS and _LDFLAGS and _LIBS
dnl
dnl Outputs:
dnl
dnl - Set $2_CPPFLAGS necessary to compile with libnl-3
dnl - Set $2_LDFLAGS necessary to link with libnl-3
dnl - Set $2_LIBS necessary to link with libnl-3
dnl - Set OPAL_HAVE_LIBNL3 1 if libnl-3 will be used
dnl
AC_DEFUN([OPAL_CHECK_LIBNL_V3],[
    OPAL_VAR_SCOPE_PUSH([CPPFLAGS_save opal_tmp_CPPFLAGS LIBS_save LDFLAGS_save])
    AC_MSG_NOTICE([checking for libnl v3])

    AS_IF([test "$opal_want_libnl" != "no"],
          [AS_IF([test -z "$opal_libnl_location"],
                 [AC_MSG_CHECKING([for /usr/include/libnl3])
                  AS_IF([test -d "/usr/include/libnl3"],
                        [opal_tmp_CPPFLAGS=-I/usr/include/libnl3
                         opal_libnlv3_happy=1
                         AC_MSG_RESULT([found])],
                        [AC_MSG_RESULT([not found])
                         AC_MSG_CHECKING([for /usr/local/include/libnl3])
                         AS_IF([test -d "/usr/local/include/libnl3"],
                               [opal_tmp_CPPFLAGS=-I/usr/local/include/netlink3
                                opal_libnlv3_happy=1
                                AC_MSG_RESULT([found])],
                               [opal_libnlv3_happy=0
                                AC_MSG_RESULT([not found])])])],
                 [AC_MSG_CHECKING([for $1/include/libnl3])
                  AS_IF([test -d "$1/include/libnl3"],
                        [opal_tmp_CPPFLAGS="-I$1/include/libnl3"
                         opal_libnlv3_happy=1
                         AC_MSG_RESULT([found])],
                        [opal_libnlv3_happy=0
                         AC_MSG_RESULT([not found])])])
           CPPFLAGS_save=$CPPFLAGS
           CPPFLAGS="$opal_tmp_CPPFLAGS $CPPFLAGS"

           # Random note: netlink/version.h is only in libnl v3 - it is not in libnl v1.
           # Also, nl_recvmsgs_report is only in libnl v3.
           AS_IF([test $opal_libnlv3_happy -eq 1],
                 [OPAL_CHECK_PACKAGE([$2],
                                     [netlink/version.h],
                                     [nl-3],
                                     [nl_recvmsgs_report],
                                     [],
                                     [$1],
                                     [],
                                     [],
                                     [opal_libnlv3_happy=0])

                  # Note that OPAL_CHECK_PACKAGE is going to add
                  # -I$dir/include into $2_CPPFLAGS.  But because libnl v3
                  # puts the headers in $dir/include/libnl3, we need to
                  # overwrite $2_CPPFLAGS with -I$dir/include/libnl3.  We can do
                  # this unconditionally; we don't have to check for
                  # success (checking for success occurs below).
                  $2_CPPFLAGS=$opal_tmp_CPPFLAGS])

           # If we found libnl-3, we *also* need libnl-route-3
           LIBS_save=$LIBS
           LDFLAGS_save=$LDFLAGS
           AS_IF([test -n "$$2_LDFLAGS"],
                 [LDFLAGS="$$2_LDFLAGS $LDFLAGS"])
           AS_IF([test $opal_libnlv3_happy -eq 1],
                 [AC_SEARCH_LIBS([nl_rtgen_request],
                                 [nl-route-3],
                                 [],
                                 [opal_libnlv3_happy=0])])
           LIBS=$LIBS_save
           LDFLAGS=$LDFLAGS_save

           # Just because libnl* is evil, double check that the
           # netlink/version.h we found was for libnl v3.  As far as we
           # know, netlink/version.h only first appeared in version
           # 3... but let's really be sure.
           AS_IF([test $opal_libnlv3_happy -eq 1],
                 [AC_MSG_CHECKING([to ensure these really are libnl v3 headers])
                  AS_IF([test -n "$$2_CPPFLAGS"],
                        [CPPFLAGS="$$2_CPPFLAGS $CPPFLAGS"])
                  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <netlink/netlink.h>
#include <netlink/version.h>
#ifndef LIBNL_VER_MAJ
#error "LIBNL_VER_MAJ not defined!"
#endif
/* to the best of our knowledge, version.h only exists in libnl v3 */
#if LIBNL_VER_MAJ != 3
#error "LIBNL_VER_MAJ != 3, I am sad"
#endif
                                     ]])],
                                    [AC_MSG_RESULT([yes])],
                                    [AC_MSG_RESULT([no])
                                     opal_libnlv3_happy=0])])

           CPPFLAGS=$CPPFLAGS_save],

          [opal_libnlv3_happy=0])

    # If we found everything
    AS_IF([test $opal_libnlv3_happy -eq 1],
          [$2_LIBS="-lnl-3 -lnl-route-3"
           OPAL_HAVE_LIBNL3=1])

   OPAL_VAR_SCOPE_POP
])

dnl
dnl Check for libnl.
dnl
dnl Inputs:
dnl
dnl $1: prefix where to look for libnl
dnl $2: var name prefix of _CPPFLAGS and _LDFLAGS and _LIBS
dnl
dnl Outputs:
dnl
dnl - Set $2_CPPFLAGS necessary to compile with libnl
dnl - Set $2_LDFLAGS necessary to link with libnl
dnl - Set $2_LIBS necessary to link with libnl
dnl - Set OPAL_HAVE_LIBNL3 0 if libnl will be used
dnl
AC_DEFUN([OPAL_CHECK_LIBNL_V1],[
    AC_MSG_NOTICE([checking for libnl v1])

    AS_IF([test "$opal_want_libnl" != "no"],
          [OPAL_CHECK_PACKAGE([$2],
                              [netlink/netlink.h],
                              [nl],
                              [nl_connect],
                              [-lm],
                              [$1],
                              [],
                              [opal_libnlv1_happy=1],
                              [opal_libnlv1_happy=0])],
          [opal_libnlv1_happy=0])

    AS_IF([test $opal_libnlv1_happy -eq 1],
          [$2_LIBS="-lnl -lm"
           OPAL_HAVE_LIBNL3=0])
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
           AC_ERROR([This is a configuration that is known to cause run-time crashes])])
])
