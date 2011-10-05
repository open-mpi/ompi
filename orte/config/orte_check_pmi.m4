# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# ORTE_CHECK_PMI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_PMI],[
	AC_ARG_WITH([pmi],
                    [AC_HELP_STRING([--with-pmi],
                            [Build PMI support (default: no)])],
		    [], with_pmi=no)
	orte_enable_pmi=0

	AC_MSG_CHECKING([if user requested PMI support])
	AS_IF([test "$with_pmi" = "no"], [
		AC_MSG_RESULT([no])
		orte_want_pmi_support=no],[
		AC_MSG_RESULT([yes])
		AS_IF([test ! -z "$with_pmi" -a "$with_pmi" != "yes"],[
			AS_IF([test -d "$with_pmi/lib64"], LDFLAGS="$LDFLAGS -L$with_pmi/lib64",
			    LDFLAGS="$LDFLAGS -L$with_pmi/lib")
			CFLAGS="$CFLAGS -I$with_pmi/include"],[])

		orte_want_pmi_support=yes
		orte_have_pmi_support=no
		AC_MSG_CHECKING([if PMI support installed])
		AC_CHECK_HEADERS([pmi.h slurm/pmi.h],[orte_have_pmi_support=yes] [break],[],[AC_INCLUDES_DEFAULT])
		AC_CHECK_LIB([pmi],[PMI_Init],[],orte_have_pmi_support=no)
	        AS_IF([test "$orte_have_pmi_support" = "yes"], [
			AC_MSG_RESULT([yes])
			orte_enable_pmi=1] [$2],[
			AC_MSG_RESULT([no])
			AC_MSG_WARN([PMI support requested (via --with-pmi) but not found.])
			AC_MSG_ERROR([Aborting.])] [$3])])
   AC_DEFINE_UNQUOTED([WANT_PMI_SUPPORT],
                      [$orte_enable_pmi],
                      [Whether we want PMI support])
])
