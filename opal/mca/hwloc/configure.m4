dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# There will only be one component used in this framework, and it will
# be selected at configure time by priority.  Components must set
# their priorities in their configure.m4 files.  They must also set
# the shell variable $opal_hwloc_base_include to a header file
# name (relative to the top OMPI source directory) that will be
# included in opal/mca/hwloc/hwloc.h.

dnl We only want one winning component (vs. STOP_AT_FIRST_PRIORITY,
dnl which will allow all components of the same priority who succeed to
dnl win)
m4_define(MCA_opal_hwloc_CONFIGURE_MODE, STOP_AT_FIRST)

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
    opal_hwloc_base_include=

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

    # Whether to enable or disable PCI support in embedded hwloc
    # support.
    AC_ARG_ENABLE([hwloc-pci],
        AC_HELP_STRING([--enable-hwloc-pci],
                       [When building the embedded hwloc, whether to explicitly enable or disable PCI device support.  By default Open MPI will build support for PCI device detection if libpciaccess is available (including its header files)]))

    AC_MSG_CHECKING([whether to enable hwloc PCI device support])
    AS_IF([test "$enable_hwloc_pci" = "yes"],
          [AC_MSG_RESULT([yes (--enable-hwloc-pci specified)])
           enable_pci=yes],
          [AS_IF([test "$enable_hwloc_pci" = "no"],
                 [AC_MSG_RESULT([no (--disable-hwloc-pci specified)])
                  enable_pci=no],
                 [AC_MSG_RESULT([yes (default)])
                  enable_pci=])
          ])

    # set defaults of not having any support
    opal_hwloc_base_enable_xml=0
    OPAL_HAVE_HWLOC=0

    # Configure all the components - always have to do this, even if
    # we configure --without-hwloc.  Note that instead of passing in
    # the traditional $1 and $2 as the first arguments, we hard-code
    # "opal" and "hwloc", because this macro is invoked via AC
    # REQUIRE.
    MCA_CONFIGURE_FRAMEWORK([opal], [hwloc], 1)

    # Restore the --enable-pci flag
    enable_pci=$opal_hwloc_hwloc132_save_enable_pci

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
   ])

   # If we have a winning component, do some more logic
   AS_IF([test "$MCA_opal_hwloc_STATIC_COMPONENTS" != ""],
       [ # We had a winner -- w00t!
        OPAL_HAVE_HWLOC=1

        # The winning component will have told us where their header file
        # is located
        AC_MSG_CHECKING([for winning hwloc component header file])
        AS_IF([test "$opal_hwloc_base_include" = ""],
              [AC_MSG_RESULT([missing])
               AC_MSG_WARN([Missing implementation header])
               AC_MSG_ERROR([Cannot continue])])
        AC_MSG_RESULT([$opal_hwloc_base_include])
        AC_DEFINE_UNQUOTED([MCA_hwloc_IMPLEMENTATION_HEADER],
                           ["$opal_hwloc_base_include"],
                           [Header to include for hwloc implementation])

        # If we added any -L flags to ADD_LDFLAGS, then we (might)
        # need to add those directories to LD_LIBRARY_PATH.
        # Otherwise, if we try to AC RUN_IFELSE anything here in
        # configure, it might die because it can't find the libraries
        # we just linked against.
        OPAL_VAR_SCOPE_PUSH([opal_hwloc_base_found_l opal_hwloc_base_token opal_hwloc_base_tmp opal_hwloc_base_dir])
        opal_hwloc_base_found_l=0
        eval "opal_hwloc_base_tmp=\$opal_hwloc_${opal_hwloc_winner}_ADD_LIBS"
        for opal_hwloc_base_token in $opal_hwloc_base_tmp; do
            case $opal_hwloc_base_token in
            -l*) opal_hwloc_base_found_l=1 ;;
            esac
        done
        AS_IF([test $opal_hwloc_base_found_l -eq 1],
              [eval "opal_hwloc_base_tmp=\$opal_hwloc_${opal_hwloc_winner}_ADD_LDFLAGS"
               for opal_hwloc_base_token in $opal_hwloc_base_tmp; do
                   case $opal_hwloc_base_token in
                   -L*)
                       opal_hwloc_base_dir=`echo $opal_hwloc_base_token | cut -c3-`
                       export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$opal_hwloc_base_dir
                       AC_MSG_WARN([Adding to LD_LIBRARY_PATH: $opal_hwloc_base_dir])
                       ;;
                   esac
               done])
        OPAL_VAR_SCOPE_POP
    ])

    AM_CONDITIONAL(OPAL_HAVE_HWLOC, test $OPAL_HAVE_HWLOC -eq 1)
    AC_DEFINE_UNQUOTED(OPAL_HAVE_HWLOC, $OPAL_HAVE_HWLOC,
        [Whether we have hwloc support or not])

   # Similar to above, if this m4 is being invoked "early" via AC
   # REQUIRE, print out a nice banner that we have now finished
   # pre-emption and are returning to the Normal Order Of Things.
   AS_IF([test "$opal_hwloc_its_time_to_configure" != "1"],
         [echo " "
          echo "<== Pre-emptive hwloc framework configuration complete."
          echo "<== We now return you to your regularly scheduled programming."
          echo " "]);
])
