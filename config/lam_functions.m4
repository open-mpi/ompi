dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2003 The Trustees of Indiana University.  
dnl                    All rights reserved.
dnl 
dnl This file is part of the LAM software package.  For license
dnl information, see the LICENSE file in the top level directory of the
dnl LAM source distribution.
dnl
dnl $Id: lam_functions.m4,v 1.1 2003/12/22 16:29:11 twoodall Exp $
dnl

AC_DEFUN(LAM_CONFIGURE_SETUP,[

# Some helper script functions.  Unfortunately, we cannot use $1 kinds
# of arugments here because of the m4 substitution.  So we have to set
# special variable names before invoking the function.  :-\

lam_show_title() {
  cat <<EOF

============================================================================
== ${1}
============================================================================
EOF
}


lam_show_subtitle() {
  cat <<EOF

*** ${1}
EOF
}

#
# Save some stats about this build
#

LAM_CONFIGURE_USER="`whoami`"
LAM_CONFIGURE_HOST="`hostname | head -n 1`"
LAM_CONFIGURE_DATE="`date`"

#
# Make automake clean emacs ~ files for "make clean"
#

CLEANFILES="*~ .\#*"
AC_SUBST(CLEANFILES)])dnl
