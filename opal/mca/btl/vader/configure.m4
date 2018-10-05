# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_vader_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_vader_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/vader/Makefile])

    OPAL_VAR_SCOPE_PUSH([btl_vader_xpmem_happy btl_vader_cma_happy btl_vader_knem_happy])

    # check for fbox support
    AC_ARG_ENABLE([vader-fbox-support],
		  [AC_HELP_STRING([--disable-vader-fbox-support],
				  [Disable fastbox support in vader shared memory btl (default: enabled)])])
    AC_MSG_CHECKING([whether to enable fastbox support])
    AS_IF([test "$enable_vader_fbox_support" != "no"],
	  [AC_MSG_RESULT([yes])
	   OPAL_BTL_VADER_FBOX_SUPPORT=1],
	  [AC_MSG_RESULT([no])
	   OPAL_BTL_VADER_FBOX_SUPPORT=0])
    AC_DEFINE_UNQUOTED([OPAL_BTL_VADER_FBOX_SUPPORT],
		       [$OPAL_BTL_VADER_FBOX_SUPPORT],
		       [Enable fastbox support in vader btl])

    # Check for single-copy APIs

    OPAL_CHECK_XPMEM([btl_vader], [btl_vader_xpmem_happy=1], [btl_vader_xpmem_happy=0])
    OPAL_CHECK_KNEM([btl_vader], [btl_vader_knem_happy=1],[btl_vader_knem_happy=0])
    OPAL_CHECK_CMA([btl_vader], [AC_CHECK_HEADER([sys/prctl.h]) btl_vader_cma_happy=1], [btl_vader_cma_happy=0])

    AC_DEFINE_UNQUOTED([OPAL_BTL_VADER_HAVE_XPMEM], [$btl_vader_xpmem_happy],
        [If XPMEM support can be enabled within vader])

    AC_DEFINE_UNQUOTED([OPAL_BTL_VADER_HAVE_CMA], [$btl_vader_cma_happy],
        [If CMA support can be enabled within vader])

    AC_DEFINE_UNQUOTED([OPAL_BTL_VADER_HAVE_KNEM], [$btl_vader_knem_happy],
	[If KNEM support can be enabled within vader])

    OPAL_VAR_SCOPE_POP

    # always happy
    [$1]

    OPAL_SUMMARY_ADD([[Transports]],[[Shared memory/copy in+copy out]],[$1],[yes])

# substitute in the things needed to build with XPMEM support
    AC_SUBST([btl_vader_CFLAGS])
    AC_SUBST([btl_vader_CPPFLAGS])
    AC_SUBST([btl_vader_LDFLAGS])
    AC_SUBST([btl_vader_LIBS])
])dnl
