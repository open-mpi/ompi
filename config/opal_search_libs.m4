dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
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
    m4_ifdef([mca_component_configure_active],
        [m4_fatal([*** OPAL_SEARCH_LIBS_CORE cannot be called from a component configure.m4])])

    OPAL_VAR_SCOPE_PUSH([LIBS_save add])
    LIBS_save=$LIBS

    AC_SEARCH_LIBS([$1], [$2],
        [ # Found it!  See if anything was added to LIBS
         add=`printf '%s\n' "$LIBS" | sed -e "s/$LIBS_save$//"`
         AS_IF([test -n "$add"],
             [OPAL_WRAPPER_FLAGS_ADD([LIBS], [$add])])
         opal_have_$1=1
         $3],
        [opal_have_$1=0
         $4], [$5])

    AC_DEFINE_UNQUOTED([OPAL_HAVE_]m4_toupper($1), [$opal_have_$1],
         [whether $1 is found and available])

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
    m4_ifndef([mca_component_configure_active],
        [m4_fatal([*** OPAL_SEARCH_LIBS_COMPONENT can only be called from a component configure.m4])])

    OPAL_VAR_SCOPE_PUSH([LIBS_save add])
    LIBS_save=$LIBS

    AC_SEARCH_LIBS([$2], [$3],
        [ # Found it!  See if anything was added to LIBS
         add=`printf '%s\n' "$LIBS" | sed -e "s/$LIBS_save$//"`
         AS_IF([test -n "$add"],
             [OPAL_FLAGS_APPEND_UNIQ($1_LIBS, [$add])])
         $1_have_$2=1
         $4],
        [$1_have_$2=0
         $5], [$6])

        AC_DEFINE_UNQUOTED([OPAL_HAVE_]m4_toupper($1), [$$1_have_$2],
            [whether $1 is found and available])
    OPAL_VAR_SCOPE_POP
])dnl
