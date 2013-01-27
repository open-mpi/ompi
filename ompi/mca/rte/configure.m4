dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2012      Los Alamos National Security, LLC.  All rights reserved.

dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# There will only be one component used in this framework, and it will
# be selected at configure time by priority.  If a component succeeds,
# it must also set the shell variable ompi_rte_base_include to the
# path from <top_srcdir>/ompi/mca/rte/ to the header containing the
# RTE interface.

dnl We only want one winning component.
m4_define(MCA_ompi_rte_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_ompi_rte_CONFIG],[
    ompi_rte_base_include=

    # configure all the components
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    AS_IF([test "$ompi_rte_base_include" = ""],
          [AC_MSG_ERROR([Did not find a suitable rte component])])

    AC_DEFINE_UNQUOTED([MCA_rte_IMPLEMENTATION_HEADER],
                       ["ompi/mca/rte/$ompi_rte_base_include"],
                       [Header to include for rte implementation])
])
