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
# the shell variable $opal_hwloc_<component>_include to a header file
# name (relative to the top OMPI source directory) that will be
# included in opal/mca/hwloc/hwloc.h.

# Optionally, components may also set the shell variable
# $opal_hwloc_<component>_cppflags if additional CPPFLAGS must be used
# with this header file, and $opal_hwloc_<component>_ldflags and
# $opal_hwloc_<component>_libs.  The hwloc framework will add the
# winning component's $opal_hwloc_<c,oponent>_* to CPPFLAGS, LDFLAGS,
# and LIBS, respectively.

# Finally, components set $opal_hwloc_<component>_enable_xml to 0 or
# 1, indicating whether they have XML support or not.

dnl We only want one winning component.
m4_define(MCA_opal_hwloc_CONFIGURE_MODE, STOP_AT_FIRST_PRIORITY)

# Other components may depend on at least 1 hwloc component being
# available.  As such, we may need to artificially force this
# framework to be configured first.  Hence, we move the entirety of
# the hwloc framework's m4 to a separate macro and AC REQUIRE it.
# Other components can do this as well.  This will guarantee that
# OPAL_HAVE_HWLOC is set to 0 or 1 *before* some component needs to
# check it.

AC_DEFUN([MCA_opal_hwloc_CONFIG],[
    # Use a crude shell variable to know whether this component is
    # being required "early".  See below.
    opal_hwloc_its_time_to_configure=1
    AC_REQUIRE([MCA_opal_hwloc_CONFIG_REQUIRE])
])

# See comments above for why this is a separate macro.

AC_DEFUN([MCA_opal_hwloc_CONFIG_REQUIRE],[

   # If this shell variable is not 1, then this m4 is being invoked
   # "early" via AC REQUIRE.  Therefore, since we like having fairly
   # readable configure output, print out a nice banner explaining why
   # this is coming early.
   AS_IF([test "$opal_hwloc_its_time_to_configure" != "1"],
         [echo " "
          echo "==> Pre-emptively configuring the hwloc framework to satisfy dependencies."])

    # See if we want hwloc, and if so, internal vs external
    AC_ARG_WITH(hwloc,
        AC_HELP_STRING([--with-hwloc(=DIR)],
                       [Build hwloc support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of hwloc.  "external" forces Open MPI to use an external installation of hwloc.  Supplying a valid directory name also forces Open MPI to use an external installation of hwloc, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries.]))

    # set defaults of not having any support
    opal_hwloc_base_enable_xml=0
    OPAL_HAVE_HWLOC=0

    # Configure all the components - always have to do this, even if
    # we configure --without-hwloc.  Note that instead of passing in
    # the traditional $1 and $2 as the first arguments, we hard-code
    # "opal" and "hwloc", because this macro is invoked via AC
    # REQUIRE.
    MCA_CONFIGURE_FRAMEWORK([opal], [hwloc], 1)

    # Strip any leading/trailing spaces
    opal_hwloc_winner=`echo $MCA_opal_hwloc_STATIC_COMPONENTS | sed -e 's/^[ ]+//' | sed -e 's/[ ]+$//'`

    # Give a blank line to separate these messages from the last
    # component's configure.m4 output.
    echo " "

    # Unless --with-hwloc[=<foo>] was given, it's ok to have no hwloc
    # component.
    AS_IF([test "$with_hwloc" = "no" -o "$with_hwloc" = ""], [],
       [ # STOP_AT_FIRST_PRIORITY will guarantee that we find at most
        # one.  We need to check here that we found *at least* one.
        AS_IF([test "$MCA_opal_hwloc_STATIC_COMPONENTS" = ""],
              [AC_MSG_WARN([Did not find a suitable static opal hwloc component])
               AC_MSG_ERROR([Cannot continue])])

        # If there's any spaces in the middle of the string, then we
        # found more than 1 eligible static component.  That's no good
        # (and should never happen, but let's be sure)!
        AS_IF([test "`echo $opal_hwloc_winner | sed 's/ //'`" != "$opal_hwloc_winner"],
              [AC_MSG_WARN([Found more than 1 eligible static opal hwloc component])
               AC_MSG_WARN([This should never happen!])
               AC_MSG_ERROR([Cannot continue])])
   ])

   # If we have a winning component, do some more logic
   AS_IF([test "$MCA_opal_hwloc_STATIC_COMPONENTS" != ""],
       [ # We had a winner -- w00t!
        OPAL_HAVE_HWLOC=1

        # The winning component will have told us where their header file
        # is located
        AC_MSG_CHECKING([for winning hwloc component header file])
        eval "opal_hwloc_base_include=\`echo \$opal_hwloc_${opal_hwloc_winner}_include\`"
        AS_IF([test "$opal_hwloc_base_include" = ""],
              [AC_MSG_RESULT([missing])
               AC_MSG_WARN([Missing implementation header])
               AC_MSG_ERROR([Cannot continue])])
        AC_MSG_RESULT([$opal_hwloc_base_include])

        AC_DEFINE_UNQUOTED([MCA_hwloc_IMPLEMENTATION_HEADER],
                           ["$opal_hwloc_base_include"],
                           [Header to include for hwloc implementation])

        # See if they set any flags for us
        AC_MSG_CHECKING([for winning hwloc component additional CPPFLAGS])
        eval "opal_hwloc_base_cppflags=\`echo \$opal_hwloc_${opal_hwloc_winner}_cppflags\`"
        AS_IF([test "$opal_hwloc_base_cppflags" != ""],
              [AC_MSG_RESULT([$opal_hwloc_base_cppflags])
               CPPFLAGS="$CPPFLAGS $opal_hwloc_base_cppflags"],
              [AC_MSG_RESULT([none])])
        AC_MSG_CHECKING([for winning hwloc component additional LDFLAGS])
        eval "opal_hwloc_base_ldflags=\`echo \$opal_hwloc_${opal_hwloc_winner}_ldflags\`"
        AS_IF([test "$opal_hwloc_base_ldflags" != ""],
              [AC_MSG_RESULT([$opal_hwloc_base_ldflags])
               LDFLAGS="$LDFLAGS $opal_hwloc_base_ldflags"],
              [AC_MSG_RESULT([none])])
        AC_MSG_CHECKING([for winning hwloc component additional LIBS])
        eval "opal_hwloc_base_libs=\`echo \$opal_hwloc_${opal_hwloc_winner}_libs\`"
        AS_IF([test "$opal_hwloc_base_libs" != ""],
              [AC_MSG_RESULT([$opal_hwloc_base_libs])
               LIB="$LIBS $opal_hwloc_base_libs"],
              [AC_MSG_RESULT([none])])

        AC_MSG_CHECKING([if winning hwloc supports XML])
        eval "opal_hwloc_base_enable_xml=\`echo \$opal_hwloc_${opal_hwloc_winner}_enable_xml\`"
        AS_IF([test "$opal_hwloc_base_enable_xml" = "1"],
              [AC_MSG_RESULT([yes])],
              [AC_MSG_RESULT([no])])
    ])

    AM_CONDITIONAL(OPAL_HAVE_HWLOC, test $OPAL_HAVE_HWLOC -eq 1)
    AC_DEFINE_UNQUOTED(OPAL_HAVE_HWLOC, $OPAL_HAVE_HWLOC,
        [Whether we have hwloc support or not])
    AC_DEFINE_UNQUOTED(OPAL_HAVE_HWLOC_XML, $opal_hwloc_base_enable_xml,
        [Enable xml support or not])

   # Similar to above, if this m4 is being invoked "early" via AC
   # REQUIRE, print out a nice banner that we have now finished
   # pre-emption and are returning to the Normal Order Of Things.
   AS_IF([test "$opal_hwloc_its_time_to_configure" != "1"],
         [echo " "
          echo "<== Pre-emptive hwloc framework configuration complete."
          echo "<== We now return you to your regularly scheduled programming."
          echo " "]);
])
