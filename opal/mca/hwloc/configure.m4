dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# There will only be one component used in this framework, and it will
# be selected at configure time by priority.  Components must set
# their priorities in their configure.m4 files.  They must also set
# the shell variable $hwloc_base_include to a header file name
# (relative to the top OMPI source directory) that will be included in
# opal/mca/hwloc/hwloc.h.  Optionally, components may also set the
# shell variable $hwloc_base_cppflags if additional CPPFLAGS must be
# used with this header file, and $hwloc_base_ldflags and
# $hwloc_base_libs.  The hwloc framework will add the winning
# component's $hwloc_base_* to CPPFLAGS, LDFLAGS, and LIBS,
# respectively.

# If the user specifies --without-hwloc, then:
#
# - no hwloc component will be configured
# - $OPAL_HAVE_HWLOC will be set to 0
# - OPAL_HAVE_HWLOC will be AC_DEFINE'd to 0
#
# Otherwise:
#
# - a hwloc component will be configured.  configure will abort if no
#   hwloc component is able to be configured.
# - $OPAL_HAVE_HWLOC will be set to 1
# - OPAL_HAVE_HWLOC will be AC_DEFINE'd to 1
#
# Other configury (e.g., components that depend on hwloc) can simply
# check the value of $with_hwloc.  If it's "no", then they should know
# that hwloc will not be available.  If it's not "no", then they can
# assume that hwloc will be available (and that this framework will
# abort configure if hwloc is *not* available).

dnl We only want one winning component.
m4_define(MCA_opal_hwloc_CONFIGURE_MODE, STOP_AT_FIRST_PRIORITY)

AC_DEFUN([MCA_opal_hwloc_CONFIG],[

    # See if we want hwloc, and if so, internal vs external
    AC_ARG_WITH(hwloc,
        AC_HELP_STRING([--with-hwloc(=DIR)],
                       [Build hwloc support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of hwloc.  "external" forces Open MPI to use an external installation of hwloc.  Supplying a valid directory name also forces Open MPI to use an external installation of hwloc, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries.]))

    # set default
    hwloc_base_enable_xml=0

    AC_MSG_CHECKING([want hwloc support])
    if test "$with_hwloc" = "no"; then
        AC_MSG_RESULT([no])
        OPAL_HAVE_HWLOC=0
    else
        AC_MSG_RESULT([yes])
        OPAL_HAVE_HWLOC=1
    fi

    # configure all the components - always have to do this
    MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

    if test "$with_hwloc" != "no"; then
        # We must have found exactly 1 static component, or we can't
        # continue.  STOP_AT_FIRST_PRIORITY will guarantee that we find at
        # most one.  We need to check here that we found *at least* one.
        AS_IF([test "$MCA_opal_hwloc_STATIC_COMPONENTS" = ""],
              [AC_MSG_WARN([Did not find a suitable static opal hwloc component])
               AC_MSG_ERROR([Cannot continue])])

        # The winning component will have set this.
        AS_IF([test "$hwloc_base_include" = ""],
              [AC_MSG_WARN([Missing implementation header])
               AC_MSG_ERROR([Cannot continue])])

        AC_DEFINE_UNQUOTED([MCA_hwloc_IMPLEMENTATION_HEADER],
                           ["$hwloc_base_include"],
                           [Header to include for hwloc implementation])

        # Give a blank line to separate these messages from the last
        # component's configure.m4 output.

        echo " "

        AC_MSG_CHECKING([for winning hwloc component additional CPPFLAGS])
        AS_IF([test "$hwloc_base_cppflags" != ""],
              [AC_MSG_RESULT([$hwloc_base_cppflags])
               CPPFLAGS="$CPPFLAGS $hwloc_base_cppflags"],
              [AC_MSG_RESULT([none])])
        AC_MSG_CHECKING([for winning hwloc component additional LDFLAGS])
        AS_IF([test "$hwloc_base_ldflags" != ""],
              [AC_MSG_RESULT([$hwloc_base_ldflags])
               CPPFLAGS="$CPPFLAGS $hwloc_base_ldflags"],
              [AC_MSG_RESULT([none])])
        AC_MSG_CHECKING([for winning hwloc component additional LIBS])
        AS_IF([test "$hwloc_base_libs" != ""],
              [AC_MSG_RESULT([$hwloc_base_libs])
               CPPFLAGS="$CPPFLAGS $hwloc_base_libs"],
              [AC_MSG_RESULT([none])])
    fi

    AM_CONDITIONAL(OPAL_HAVE_HWLOC, test "$with_hwloc" != "no")
    AC_DEFINE_UNQUOTED(OPAL_HAVE_HWLOC, $OPAL_HAVE_HWLOC,
        [Whether we have hwloc support or not])
    AC_DEFINE_UNQUOTED(OPAL_HAVE_HWLOC_XML, $hwloc_base_enable_xml,
        [Enable xml support or not])
])
