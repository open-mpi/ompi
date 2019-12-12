dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2017 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2019      Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# There will only be one component used in this framework, and it will
# be selected at configure time by priority.  Components must set
# their priorities in their configure.m4 files.  They must also set
# the shell variable $opal_pmix_base_include to a header file
# name (relative to the top OMPI source directory) that will be
# included in opal/mca/pmix/pmix-internal.h.

dnl We only want one winning component (vs. STOP_AT_FIRST_PRIORITY,
dnl which will allow all components of the same priority who succeed to
dnl win)
m4_define(MCA_opal_pmix_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_opal_pmix_CONFIG],[
    opal_pmix_base_include=

    # See if we want internal vs external pmix
    AC_ARG_WITH(pmix,
        AC_HELP_STRING([--with-pmix(=DIR)],
                       [Build pmix support.  DIR can take one of three values: "internal", "external", or a valid directory name.  "internal" (or no DIR value) forces Open MPI to use its internal copy of pmix.  "external" forces Open MPI to use an external installation of pmix.  Supplying a valid directory name also forces Open MPI to use an external installation of pmix, and adds DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries. Note that Open MPI no longer supports --without-pmix.]))

    # check for error
    AS_IF([test "$with_pmix" = "no"],
          [AC_MSG_WARN([Open MPI requires PMIX support. It can be built])
           AC_MSG_WARN([with either its own internal copy of PMIX, or with])
           AC_MSG_WARN([an external copy that you supply.])
           AC_MSG_ERROR([Cannot continue])])

    # Configure all the components - always have to do this. Note that
    # instead of passing in
    # the traditional $1 and $2 as the first arguments, we hard-code
    # "opal" and "pmix", because this macro is invoked via AC
    # REQUIRE.
    MCA_CONFIGURE_FRAMEWORK([opal], [pmix], 1)

    # Give a blank line to separate these messages from the last
    # component's configure.m4 output.
    echo " "

    # If we aren't given a specific component, then we must find one
    AS_IF([test "$with_pmix" = ""], [],
       [ # STOP_AT_FIRST_PRIORITY will guarantee that we find at most
        # one.  We need to check here that we found *at least* one.
        AS_IF([test "$MCA_opal_pmix_STATIC_COMPONENTS" = ""],
              [AC_MSG_WARN([Did not find a suitable static opal pmix component])
               AC_MSG_ERROR([Cannot continue])])
   ])

   # If we have a winning component, do some more logic
   AS_IF([test "$MCA_opal_pmix_STATIC_COMPONENTS" != ""],
       [ # We had a winner -- w00t!
        # The winning component will have told us where their header file
        # is located
        AC_MSG_CHECKING([for winning pmix component header file])
        AS_IF([test "$opal_pmix_base_include" = ""],
              [AC_MSG_RESULT([missing])
               AC_MSG_WARN([Missing implementation header])
               AC_MSG_ERROR([Cannot continue])])
        AC_MSG_RESULT([$opal_pmix_base_include])
        AC_DEFINE_UNQUOTED([MCA_pmix_IMPLEMENTATION_HEADER],
                           ["$opal_pmix_base_include"],
                           [Header to include for pmix implementation])

        # If we added any -L flags to ADD_LDFLAGS, then we (might)
        # need to add those directories to LD_LIBRARY_PATH.
        # Otherwise, if we try to AC RUN_IFELSE anything here in
        # configure, it might die because it can't find the libraries
        # we just linked against.
        OPAL_VAR_SCOPE_PUSH([opal_pmix_base_found_l opal_pmix_base_token opal_pmix_base_tmp opal_pmix_base_dir])
        opal_pmix_base_found_l=0
        eval "opal_pmix_base_tmp=\$opal_pmix_${opal_pmix_winner}_ADD_LIBS"
        for opal_pmix_base_token in $opal_pmix_base_tmp; do
            case $opal_pmix_base_token in
            -l*) opal_pmix_base_found_l=1 ;;
            esac
        done
        AS_IF([test $opal_pmix_base_found_l -eq 1],
              [eval "opal_pmix_base_tmp=\$opal_pmix_${opal_pmix_winner}_ADD_LDFLAGS"
               for opal_pmix_base_token in $opal_pmix_base_tmp; do
                   case $opal_pmix_base_token in
                   -L*)
                       opal_pmix_base_dir=`echo $opal_pmix_base_token | cut -c3-`
                       export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$opal_pmix_base_dir
                       AC_MSG_WARN([Adding to LD_LIBRARY_PATH: $opal_pmix_base_dir])
                       ;;
                   esac
               done])
        OPAL_VAR_SCOPE_POP
    ])

   # Similar to above, if this m4 is being invoked "early" via AC
   # REQUIRE, print out a nice banner that we have now finished
   # pre-emption and are returning to the Normal Order Of Things.
   AS_IF([test "$opal_pmix_its_time_to_configure" != "1"],
         [echo " "
          echo "<== Pre-emptive pmix framework configuration complete."
          echo "<== We now return you to your regularly scheduled programming."
          echo " "]);
])
