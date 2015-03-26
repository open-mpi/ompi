dnl
dnl Copyright (c) 2015, Cisco Systems, Inc. All rights reserved.
dnl
dnl This software is available to you under a choice of one of two
dnl licenses.  You may choose to be licensed under the terms of the GNU
dnl General Public License (GPL) Version 2, available from the file
dnl COPYING in the main directory of this source tree, or the
dnl BSD license below:
dnl
dnl     Redistribution and use in source and binary forms, with or
dnl     without modification, are permitted provided that the following
dnl     conditions are met:
dnl
dnl      - Redistributions of source code must retain the above
dnl        copyright notice, this list of conditions and the following
dnl        disclaimer.
dnl
dnl      - Redistributions in binary form must reproduce the above
dnl        copyright notice, this list of conditions and the following
dnl        disclaimer in the documentation and/or other materials
dnl        provided with the distribution.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
dnl "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
dnl LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
dnl FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
dnl COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
dnl INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
dnl BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
dnl LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
dnl CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
dnl LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
dnl ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
dnl POSSIBILITY OF SUCH DAMAGE.
dnl

dnl Configury specific to the libfabric usNIC provider

dnl libnl is sadness, but we have to use it.  The majority of this
dnl configure.m4 is just to deal with libnl.  :-(

dnl libnl has two versions: libnl (i.e., version 1) and libnl3.

dnl These two versions have many of the same symbols, but they are
dnl incompatible with each other.  We can handle this in the C code, but
dnl we must know which version to compile file (i.e., configure must
dnl figure this out).  Additionally, if both versions get linked into
dnl the same process, they will disrupt each other's global state, and
dnl Random Bad Things happen.  We can't always prevent this -- e.g., if we
dnl link against libnl vX and some other middleware links against libnl vY
dnl (and X != Y), prepare for unpleasentness.  You have been warned.

dnl As of this writing (March 2015), most Linux distros seem to be
dnl encouraging packages to prefer libnl v3 over libnl v1.

dnl libnl wants us to use pkg-config to find CPPFLAGS and LDFLAGS and
dnl LIBS, but pkg-config isn't always available.  So we have to test here.
dnl It gets more complicated because libnl changed several things between v1
dnl and v3:

dnl v1:
dnl - Header files (e.g., <netlink/netlink.h> are in $prefix/include
dnl - Library is in $prefix/lib[64]
dnl - Library is named libnl.<suffix>

dnl v3:
dnl - Header files (e.g., <netlink/netlink.h> are in $prefix/include/libnl3
dnl   *** NOTE: This means that a -I<dir> switch is REQUIRED to find
dnl             the libnl3 headers (!)
dnl - Library is in $prefix/lib[64]
dnl - Library is named libnl-3.<suffix>
dnl - We *also* need the libnl-route-3 library

dnl These differing requirements make the configure/m4 tests a bit of
dnl a nightmare.  :-(

dnl ---------------------------------------------------------------------------

dnl This configure.m4 script supports the following CLI options:

dnl --with-libnl[=dir]
dnl If specified, look for libnl support.  If it is not found,
dnl error/abort configure.  If dir is specified, look in that
dnl directory (configure will first look for libnl v3 in that tree, and if
dnl it is not found, look for libnl v1 in that tree).  If no dir is
dnl specified, this option is redundant with --with-usnic.

dnl --without-libnl
dnl Do not look for libnl support.  This means that the usnic provider
dnl will not be built (since the usnic provider *requires* libnl support).

dnl ---------------------------------------------------------------------------

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
	  [AC_CHECK_HEADER([infiniband/verbs.h], [usnic_happy=1])
	   AS_IF([test $usnic_happy -eq 1],
	       [USNIC_CHECK_LIBNL_SADNESS])
	  ])
])

dnl
dnl Helper function to parse --with-libnl* options
dnl
dnl $1: variable name
dnl $2: --with-<foo> value
dnl
AC_DEFUN([USNIC_PARSE_WITH],[
	case "$2" in
	no)
		# Nope, don't want it
		usnic_want_$1=no
		;;
	yes)
		# Yes, definitely want it
		usnic_want_$1=yes
		;;
	default)
		# Default case -- try and see if we can find it
		usnic_want_$1=default
		usnic_$1_location=/usr
		;;
	*)
		# Yes, definitely want it -- at a specific location
		usnic_want_$1=yes
		usnic_$1_location="$2"
		;;
	esac
])

dnl
dnl Shared macro
dnl
AC_DEFUN([USNIC_CHECK_LIBNL_SADNESS],[
	AC_ARG_WITH([libnl],
		[AC_HELP_STRING([--with-libnl(=DIR)],
			[Directory prefix for libnl (typically only necessary if libnl is installed in a location that the compiler/linker will not search by default)])],
		[], [with_libnl=default])

	# The --with options carry two pieces of information: 1) do
	# you want a specific version of libnl, and 2) where that
	# version of libnl lives.  For simplicity, let's separate
	# those two pieces of information.
	USNIC_PARSE_WITH([libnl], [$with_libnl])

	# Default to a numeric value (this value gets AC_DEFINEd)
	HAVE_LIBNL3=0

	###################################################
	# NOTE: We *must* check for libnl3 before libnl.
	###################################################

	AS_IF([test "$usnic_want_libnl" != "no"],
	      [USNIC_CHECK_LIBNL3([$usnic_libnl_location], [usnic_nl])])
	AS_IF([test "$usnic_want_libnl" != "no" &&
	       test "$usnic_nl_LIBS" = ""],
	      [USNIC_CHECK_LIBNL([$usnic_libnl_location], [usnic_nl])])

	AS_IF([test "$usnic_want_libnl" = "yes" &&
	       test "$usnic_nl_LIBS" = ""],
	      [AC_MSG_WARN([--with-libnl specified, but not found])
	       AC_MSG_ERROR([Cannot continue])])

	# Final result
	AC_SUBST([HAVE_LIBNL3])
	AC_DEFINE_UNQUOTED([HAVE_LIBNL3], [$HAVE_LIBNL3],
	      [Whether we have libl or libnl3])

	AC_SUBST([usnic_nl_CPPFLAGS])
	AC_SUBST([usnic_nl_LDFLAGS])
	AC_SUBST([usnic_nl_LIBS])

	AS_IF([test "$usnic_nl_LIBS" = ""],
	      [usnic_happy=0])
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
dnl - Set HAVE_LIBNL3 1 if libnl-3 will be used
dnl
AC_DEFUN([USNIC_CHECK_LIBNL3],[
	AC_MSG_NOTICE([checking for libnl3])

	AC_MSG_CHECKING([for libnl3 prefix])
	AC_MSG_RESULT([$1])
	AC_MSG_CHECKING([for $1/include/libnl3])
	AS_IF([test -d "$1/include/libnl3"],
	      [usnic_libnl3_happy=1
	       AC_MSG_RESULT([found])],
	      [usnic_libnl3_happy=0
	       AC_MSG_RESULT([not found])])

	# Random note: netlink/version.h is only in libnl3 - it is not in libnl.
	# Also, nl_recvmsgs_report is only in libnl3.
	CPPFLAGS_save=$CPPFLAGS
	usnic_tmp_CPPFLAGS="-I$1/include/libnl3"
	CPPFLAGS="$usnic_tmp_CPPFLAGS $CPPFLAGS"
	AS_IF([test $usnic_libnl3_happy -eq 1],
	      [FI_CHECK_PACKAGE([$2],
				[netlink/version.h],
				[nl-3],
				[nl_recvmsgs_report],
				[],
				[$1],
				[],
				[usnic_libnl3_happy=1],
				[usnic_libnl3_happy=0])

		# Note that FI_CHECK_PACKAGE is going to add
		# -I$dir/include into $2_CPPFLAGS.  But because libnl3
		# puts the headers in $dir/libnl3, we need to
		# overwrite $2_CPPFLAGS with -I$dir/libnl3.  We can do
		# this unconditionally; we don't have to check for
		# success (checking for success occurs below).
		$2_CPPFLAGS=$usnic_tmp_CPPFLAGS])

	# If we found libnl-3, we *also* need libnl-route-3
	LIBS_save=$LIBS
	LDFLAGS_save=$LDFLAGS
	AS_IF([test "$$2_LDFLAGS" != ""],
	      [LDFLAGS="$$2_LDFLAGS $LDFLAGS"])
	AS_IF([test $usnic_libnl3_happy -eq 1],
	      [AC_SEARCH_LIBS([nl_rtgen_request],
			      [nl-route-3],
			      [usnic_libnl3_happy=1],
			      [usnic_libnl3_happy=0])])
	LIBS=$LIBS_save
	LDFLAGS=$LDFLAGS_save

	# Just because libnl* is evil, double check that the
	# netlink/version.h we found was for libnl3.  As far as we
	# know, netlink/version.h only first appeared in version
	# 3... but let's really be sure.
	AS_IF([test $usnic_libnl3_happy -eq 1],
	      [AC_MSG_CHECKING([to ensure these really are libnl3 headers])
	       CPPFLAGS="$$2_CPPFLAGS $CPPFLAGS"
	       AC_COMPILE_IFELSE(
			[AC_LANG_PROGRAM([[
#include <netlink/netlink.h>
#include <netlink/version.h>
#ifndef LIBNL_VER_MAJ
#error "LIBNL_VER_MAJ not defined!"
#endif
/* to the best of our knowledge, version.h only exists in libnl3 */
#if LIBNL_VER_MAJ != 3
#error "LIBNL_VER_MAJ != 3, I am sad"
#endif
		]])],
		[AC_MSG_RESULT([yes])],
		[AC_MSG_RESULT([no])
		 usnic_libnl3_happy=0]
		)])
	CPPFLAGS=$CPPFLAGS_save

	# If we found everything
	AS_IF([test $usnic_libnl3_happy -eq 1],
	      [$2_LIBS="-lnl-3 -lnl-route-3"
	       HAVE_LIBNL3=1])
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
dnl - Set HAVE_LIBNL3 0 if libnl will be used
dnl
AC_DEFUN([USNIC_CHECK_LIBNL],[
	AC_MSG_NOTICE([checking for libnl])

	FI_CHECK_PACKAGE([$2],
			[netlink/netlink.h],
			[nl],
			[nl_connect],
			[-lm],
			[$1],
			[],
			[usnic_libnl_happy=1],
			[usnic_libnl_happy=0])

	AS_IF([test $usnic_libnl_happy -eq 1],
	      [$2_LIBS="-lnl -lm"
	       HAVE_LIBNL3=0])
])
