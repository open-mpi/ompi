dnl Configry specific to the libfabrics usNIC provider

dnl
dnl Check for libnl; prefer version 3 instead of version 1.  Abort (i.e.,
dnl AC_MSG_ERROR) if neither libnl v1 or v3 can be found.
dnl
dnl Outputs:
dnl
dnl - Set $1 to the CPPFLAGS necessary to compile with libnl
dnl - Set $2 to the LIBS necessary to link with libnl
dnl - If $3 is 1, AC_MSG_ERROR (i.e., abort) if neither libnl or
dnl   libnl3 can be found
dnl - Set HAVE_LIBNL3 to 1 if libnl3 will be used; 0 if libnl1 will be used
dnl - AC_SUBST $HAVE_LIBNL3
dnl - AC_DEFINE HAVE_LIBNL3
dnl
AC_DEFUN([CHECK_LIBNL3],[
       # More libnl v1/v3 sadness: the two versions are not compatible
       # and will not work correctly if simultaneously linked into the
       # same applications.  Unfortunately, they *will* link into the
       # same image!  On platforms like SLES 12, libibverbs depends on
       # libnl-3.so.200 and friends, while a naive implementation of
       # our configure logic would link libnl.so.1 to libdaplusnic,
       # resulting in both versions in the dependency map at the same
       # time.  As a coarse fix, just check for libnl-3 first and use
       # it if present on the system.

       # GROSS: libnl wants us to either use pkg-config (which we
       # can't assume is always present) or we need to look in a
       # particular directory for the right libnl3 include files.  For
       # now, just hard code the special path into this logic.

       save_CPPFLAGS=$CPPFLAGS
       save_LIBS=$LIBS

       $1="-I/usr/include/libnl3"
       CPPFLAGS="$$1 $CPPFLAGS"
       AC_MSG_CHECKING([for /usr/include/libnl3])
       AS_IF([test -d "/usr/include/libnl3"],
             [AC_MSG_RESULT([present])
              AC_CHECK_HEADER(
                [netlink/version.h],
                [AC_COMPILE_IFELSE(
                    [AC_LANG_PROGRAM([[
#include <netlink/netlink.h>
#include <netlink/version.h>
#ifndef LIBNL_VER_MAJ
#error "LIBNL_VER_MAJ not defined!"
#endif
/* to the best of our knowledge, version.h only exists in libnl3 */
#if LIBNL_VER_MAJ < 3
#error "LIBNL_VER_MAJ < 3, this is very unusual"
#endif
                        ]],[[/* empty body */]])],
                    [HAVE_LIBNL3=1],    dnl our program compiled
                    [HAVE_LIBNL3=0])],  dnl our program failed to compile
                [HAVE_LIBNL3=0],  dnl AC_CHECK_HEADER failed
                [#include <netlink/netlink.h>
                ])],
             [AC_MSG_RESULT([missing])
              HAVE_LIBNL3=0])  dnl "/usr/include/libnl3" doesn't exist

       # nl_recvmsgs_report is a symbol that is only present in v3
       AS_IF([test "$HAVE_LIBNL3" -eq 1],
             [AC_SEARCH_LIBS([nl_recvmsgs_report], [nl-3],
                             [# We also need libnl-route-3
                              AC_SEARCH_LIBS([nl_rtgen_request], [nl-route-3],
                                             [$2="-lnl-3 -lnl-route-3"
                                              HAVE_LIBNL3=1],
                                             [HAVE_LIBNL3=0])],
                             [HAVE_LIBNL3=0])])

       AS_IF([test "$HAVE_LIBNL3" -eq 1],
             [AC_MSG_NOTICE([using libnl-3])],
             [# restore $1 since we are falling back to libnl (v1)
              $1=""
              AC_SEARCH_LIBS([nl_connect], [nl],
                             [$2="-lnl"],
                             [AC_MSG_WARN([Cannot find libnl-3 nor libnl])
                              AS_IF([test "$3" = "1"],
                                    [AC_MSG_ERROR([Cannot continue])])
                             ])
              AC_MSG_NOTICE([using libnl (v1)])])

       # libnl_utils.h does not include configure-generated config.h,
       # so it may not see the HAVE_LIBNL3 #define.  Hence, we set
       # HAVE_LIBNL3 as both a C preprocessor macro (in case some
       # other file includes config.h before libnl_utils.h) and a
       # Makefile macro (so that the app can set HAVE_LIBNL3 via
       # CPPFLAGS).  Also, this macro may be used in multiple
       # different libraries; setting HAVE_LIBNL3 both ways lets the
       # application choose which way to set it.
       AC_SUBST([HAVE_LIBNL3])
       AC_DEFINE_UNQUOTED([HAVE_LIBNL3],[$HAVE_LIBNL3],
                          [set to 1 if should use libnl v3, set to 0 for libnl v11])

       LIBS=$save_LIBS
       AS_UNSET([save_LIBS])
       CPPFLAGS=$save_CPPFLAGS
       AS_UNSET([save_CPPFLAGS])
])

dnl Called to configure this provider
dnl
dnl Arguments:
dnl
dnl $1: action if configured successfully
dnl $2: action if not configured successfully
dnl
AC_DEFUN([FI_USNIC_CONFIGURE],[
    # Determine if we can support the usnic provider
    usnic_happy=0
    AS_IF([test "x$enable_usnic" != "xno"],
	[usnic_happy=1
	 AC_CHECK_HEADER([infiniband/verbs.h], [], [usnic_happy=0])
	 CHECK_LIBNL3([usnic_libnl_CPPFLAGS],
		      [usnic_libnl_LIBS], [0])
	 AC_SUBST(usnic_libnl_CPPFLAGS)
	 AC_SUBST(usnic_libnl_LIBS)

	 AS_IF([test "$usnic_libnl_LIBS" = ""],
	       [usnic_happy=0])
	])
])
