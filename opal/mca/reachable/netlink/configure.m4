# -*- shell-script -*-
#
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dnl
dnl Portions of this software copied from libfabric
dnl (https://github.com/ofiwg/libfabric)
dnl

dnl                       BSD license
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions
dnl are met:
dnl
dnl   * Redistributions of source code must retain the above copyright
dnl     notice, this list of conditions and the following disclaimer.
dnl
dnl   * Redistributions in binary form must reproduce the above
dnl     copyright notice, this list of conditions and the following
dnl     disclaimer in the documentation and/or other materials provided
dnl     with the distribution.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
dnl "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
dnl LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
dnl FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
dnl COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
dnl INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
dnl BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
dnl LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
dnl CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
dnl LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
dnl ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
dnl POSSIBILITY OF SUCH DAMAGE.

dnl Check for libnl; prefer version 3 instead of version 1.  Abort (i.e.,
dnl AC_MSG_ERROR) if neither libnl v1 or v3 can be found.
dnl
dnl Outputs:
dnl
dnl - Set $1 to the CPPFLAGS necessary to compile with libnl
dnl - Set $2 to the LIBS necessary to link with libnl
dnl - If $3 is 1, AC_MSG_ERROR (i.e., abort) if neither libnl or
dnl   libnl3 can be found
dnl - Set OPAL_HAVE_LIBNL3 to 1 if libnl v3 will be used; 0 if libnl v1 will be used
dnl - AC_SUBST $OPAL_HAVE_LIBNL3
dnl - AC_DEFINE OPAL_HAVE_LIBNL3
dnl
dnl --------------------------------------------------------
AC_DEFUN([OPAL_REACHABLE_NETLINK_CHECK_LIBNL_Vx],[

	# Default to a numeric value (this value gets AC_DEFINEd)
	OPAL_HAVE_LIBNL3=0

	###################################################
	# NOTE: We *must* check for libnl3 before libnl.
	###################################################

	AS_IF([test $opal_libnl_version -ne 1],
	      [OPAL_CHECK_LIBNL_V3([$opal_libnl_location], [opal_reachable_netlink])])
	AS_IF([test $opal_libnl_version -ne 3 &&
	       test -z "$opal_reachable_netlink_LIBS"],
	      [OPAL_CHECK_LIBNL_V1([$opal_libnl_location], [opal_reachable_netlink])])

	AS_IF([test "$opal_want_libnl" = "yes" &&
	       test "$opal_reachable_netlink_LIBS" = ""],
	      [AC_MSG_WARN([--with-libnl specified, but not found])
	       AC_MSG_ERROR([Cannot continue])])

	# Final result
	AC_SUBST([OPAL_HAVE_LIBNL3])
	AC_DEFINE_UNQUOTED([OPAL_HAVE_LIBNL3], [$OPAL_HAVE_LIBNL3],
	      [Whether we have libl v1 or libnl v3])

	AC_SUBST([opal_reachable_netlink_CPPFLAGS])
	AC_SUBST([opal_reachable_netlink_LDFLAGS])
	AC_SUBST([opal_reachable_netlink_LIBS])

	AS_IF([test "$opal_reachable_netlink_LIBS" = ""],
	      [opal_reachable_netlink_happy=0])
])

dnl ==============================================================

# MCA_opal_reachable_netlink_CONFIG([action-if-can-compile],
#                                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_reachable_netlink_CONFIG],[
    AC_CONFIG_FILES([opal/mca/reachable/netlink/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_reachable_netlink_happy])

    opal_reachable_netlink_happy=1
    AC_CHECK_HEADER([linux/netlink.h], [],
                    [opal_reachable_netlink_happy=0], [
#include <sys/types.h>
#include <net/if.h>
])

    AS_IF([test $opal_reachable_netlink_happy -eq 1],
          [OPAL_REACHABLE_NETLINK_CHECK_LIBNL_Vx])

    AS_IF([test $opal_reachable_netlink_happy -eq 1],
          [$1],
          [$2])

    OPAL_VAR_SCOPE_POP()
])
