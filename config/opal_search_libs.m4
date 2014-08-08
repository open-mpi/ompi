dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL SEARCH_LIBS_CORE(func, list-of-libraries,
#                       action-if-found, action-if-not-found,
#                       other-libraries)
#
# Wrapper around AC SEARCH_LIBS.  If a library ends up being added to
# $LIBS, then also add it to the wrapper LIBS list (so that it is
# added to the link command line for the static link case).
#
# NOTE: COMPONENTS SHOULD NOT USE THIS MACRO!  Components should use
# OPAL_SEARCH_LIBS_COMPONENT.  The reason why is because this macro
# calls OPAL_WRAPPER_FLAGS_ADD -- see big comment in
# opal_setup_wrappers.m4 for an explanation of why this is bad).
AC_DEFUN([OPAL_SEARCH_LIBS_CORE],[
    OPAL_VAR_SCOPE_PUSH(LIBS_save add)
    LIBS_save=$LIBS

    AC_SEARCH_LIBS([$1], [$2],
        [ # Found it!  See if anything was added to LIBS
         add=`echo $LIBS | sed -e "s/$LIBS_save$//"`
         if test "x$add" != "x"; then
             OPAL_WRAPPER_FLAGS_ADD([LIBS], [$add])
         fi
         $3],
        [$4], [$5])

    OPAL_VAR_SCOPE_POP
])dnl

# OPAL SEARCH_LIBS_COMPONENT(prefix, func, list-of-libraries,
#                            action-if-found, action-if-not-found,
#                            other-libraries)
#
# Same as OPAL SEARCH_LIBS_CORE, above, except that we don't call OPAL
# WRAPPER_FLAGS_ADD.  Instead, we add it to the ${prefix}_LIBS
# variable (i.e., $prefix is usually "framework_component", such as
# "fbtl_posix").
AC_DEFUN([OPAL_SEARCH_LIBS_COMPONENT],[
    OPAL_VAR_SCOPE_PUSH(LIBS_save add)
    LIBS_save=$LIBS

    AC_SEARCH_LIBS([$2], [$3],
        [ # Found it!  See if anything was added to LIBS
         add=`echo $LIBS | sed -e "s/$LIBS_save$//"`
         if test "x$add" != "x"; then
             OPAL_FLAGS_APPEND_UNIQ($1_LIBS, [$add])
         fi
         $4],
        [$5], [$6])

    OPAL_VAR_SCOPE_POP
])dnl
