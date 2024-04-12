dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2018 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$


dnl OAC_VAR_SCOPE_INIT
dnl
dnl Initialization macro (that is AC_REQUIREd by OAC_VAR_SCOPE_PUSH /
dnl OAC_VAR_SCOPE_POP) for the var scope subsystem.  Defines the two
dnl shell functions that implement the configure-time part of the var
dnl scope evaluation system.
AC_DEFUN([OAC_VAR_SCOPE_INIT],
[oac_var_scope_push()
{
    oac_var_scope_push_lineno=$[]1
    shift
    # First, check to see if any of these variables are already set.
    # This is a simple sanity check to ensure we're not already
    # overwriting pre-existing variables (that have a non-empty
    # value).  It's not a perfect check, but at least it's something.
    for oac_var_scope_tmp_var in $[]@; do
        AS_VAR_SET_IF([$oac_var_scope_tmp_var],
            [AS_VAR_COPY([oac_var_scope_tmp_var_val], [$oac_var_scope_tmp_var])
             m4_pattern_allow([OAC_])
             AC_MSG_ERROR([Found configure shell variable clash at line $oac_var_scope_push_lineno!
[OAC_VAR_SCOPE_PUSH] called on "$oac_var_scope_tmp_var",
but it is already defined with value "$oac_var_scope_tmp_var_val"
This usually indicates an error in configure.
Cannot continue.])
             m4_pattern_forbid([OAC_])])
    done
    AS_UNSET([oac_var_scope_push_lineno])
    AS_UNSET([oac_var_scope_tmp_var])
    AS_UNSET([oac_var_scope_tmp_var_val])
}

oac_var_scope_pop()
{
    # Iterate over all the variables and unset them all
    for oac_var_scope_tmp_var in $[]@; do
        AS_UNSET([$oac_var_scope_tmp_var])
    done
    AS_UNSET([oac_var_scope_tmp_var])
}])


dnl OAC_VAR_SCOPE_PUSH: Create a new variable scope
dnl
dnl 1 -> space seperated list of variable names to push into the new scope
dnl
dnl Scope-check that the vars in the space-separated vars list are not already
dnl in use.  Generate a configure-time error if a conflict is found.  Note that
dnl the in use check is defined as "defined", so even if a var in vars list is
dnl set outside of OAC_VAR_SCOPE_PUSH, the check will still trip.
AC_DEFUN([OAC_VAR_SCOPE_PUSH],[
AC_REQUIRE([OAC_VAR_SCOPE_INIT])dnl
m4_pushdef([oac_var_scope_stack], [$1])dnl
m4_foreach_w([oac_var_scope_var], [$1],
             [m4_set_add([oac_var_scope_active_set], oac_var_scope_var,
                         [], [m4_fatal([$0 found the variable ]oac_var_scope_var[
active in a previous scope.])])])dnl
oac_var_scope_push ${LINENO} $1
])dnl


dnl OAC_VAR_SCOPE_POP: pop off the current variable scope
dnl
dnl Unset the last set of variables set in OAC_VAR_SCOPE_POP.  Every call to
dnl OAC_VAR_SCOPE_PUSH should have a matched call to this macro.
AC_DEFUN([OAC_VAR_SCOPE_POP],[
AC_REQUIRE([OAC_VAR_SCOPE_INIT])dnl
m4_ifdef([oac_var_scope_stack], [],
         [m4_pattern_allow([OAC_])
          m4_fatal([$0 was called without a defined
variable stack.  This usually means that $0 was called more
times than OAC_VAR_SCOPE_PUSH.])
          m4_pattern_forbid([OAC_])])dnl
m4_foreach_w([oac_var_scope_var], oac_var_scope_stack,
             [m4_set_remove([oac_var_scope_active_set], oac_var_scope_var)])dnl
oac_var_scope_pop oac_var_scope_stack
m4_popdef([oac_var_scope_stack])dnl
])dnl

