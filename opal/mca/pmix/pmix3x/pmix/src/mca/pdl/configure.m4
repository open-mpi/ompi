dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2016-2017 Intel, Inc. All rights reserved.
dnl Copyright (c) 2016-2019 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl There will only be one component used in this framework, and it will
dnl be selected at configure time by priority.  Components must set
dnl their priorities in their configure.m4 file.

dnl We only want one winning component (vs. STOP_AT_FIRST_PRIORITY,
dnl which will allow all components of the same priority who succeed to
dnl win)
m4_define(MCA_pmix_pdl_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_pmix_pdl_CONFIG],[
    PMIX_HAVE_PDL_SUPPORT=0

    # If --disable-dlopen was used, then have all the components fail
    # (we still need to configure them all so that things like "make
    # dist" work", but we just want the MCA system to (artificially)
    # conclude that it can't build any of the components.
    AS_IF([test $PMIX_ENABLE_DLOPEN_SUPPORT -eq 0],
          [want_pdl=0], [want_pdl=1])

    MCA_CONFIGURE_FRAMEWORK([pdl], [$want_pdl])

    # If we found no suitable static pdl component and dlopen support
    # was not specifically disabled, this is an error.
    AS_IF([test "$MCA_pmix_pdl_STATIC_COMPONENTS" = "" && \
           test $PMIX_ENABLE_DLOPEN_SUPPORT -eq 1],
          [AC_MSG_WARN([Did not find a suitable static pmix pdl component])
           AC_MSG_WARN([You might need to install libltld (and its headers) or])
           AC_MSG_WARN([specify --disable-dlopen to configure.])
           AC_MSG_ERROR([Cannot continue])])

    # If we have a winning component (which, per above, will only
    # happen if --disable-dlopen was *not* specified), do some more
    # logic.
    AS_IF([test "$MCA_pmix_pdl_STATIC_COMPONENTS" != ""],
       [ # We had a winner -- w00t!

        PMIX_HAVE_PDL_SUPPORT=1
        # If we added any -L flags to ADD_LDFLAGS, then we (might)
        # need to add those directories to LD_LIBRARY_PATH.
        # Otherwise, if we try to AC RUN_IFELSE anything here in
        # configure, it might die because it can't find the libraries
        # we just linked against.
        PMIX_VAR_SCOPE_PUSH([pmix_pdl_base_found_l pmix_pdl_base_token pmix_pdl_base_tmp pmix_pdl_base_dir])
        pmix_pdl_base_found_l=0
        eval "pmix_pdl_base_tmp=\$pmix_pdl_${pmix_pdl_winner}_ADD_LIBS"
        for pmix_pdl_base_token in $pmix_pdl_base_tmp; do
            case $pmix_pdl_base_token in
            -l*) pmix_pdl_base_found_l=1 ;;
            esac
        done
        AS_IF([test $pmix_pdl_base_found_l -eq 1],
              [eval "pmix_pdl_base_tmp=\$pmix_pdl_${pmix_pdl_winner}_ADD_LDFLAGS"
               for pmix_pdl_base_token in $pmix_pdl_base_tmp; do
                   case $pmix_pdl_base_token in
                   -L*)
                       pmix_pdl_base_dir=`echo $pmix_pdl_base_token | cut -c3-`
                       export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$pmix_pdl_base_dir
                       AC_MSG_WARN([Adding to LD_LIBRARY_PATH: $pmix_pdl_base_dir])
                       ;;
                   esac
               done])
        PMIX_VAR_SCOPE_POP
    ])

    AC_DEFINE_UNQUOTED([PMIX_HAVE_PDL_SUPPORT], [$PMIX_HAVE_PDL_SUPPORT],
                       [Whether the PMIX PDL framework is functional or not])
])
