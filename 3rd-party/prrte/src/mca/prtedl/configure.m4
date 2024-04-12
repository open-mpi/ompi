dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
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
m4_define(MCA_prte_prtedl_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_prte_prtedl_CONFIG],[
    PRTE_HAVE_DL_SUPPORT=0

    # If --disable-dlopen was used, then have all the components fail
    # (we still need to configure them all so that things like "make
    # dist" work", but we just want the MCA system to (artificially)
    # conclude that it can't build any of the components.
    AS_IF([test $PRTE_ENABLE_DLOPEN_SUPPORT -eq 0],
          [want_prtedl=0], [want_prtedl=1])

    MCA_CONFIGURE_FRAMEWORK([prtedl], [$want_prtedl])

    # If we found no suitable static prtedl component and dlopen support
    # was not specifically disabled, this is an error.
    AS_IF([test "$MCA_prte_prtedl_STATIC_COMPONENTS" = "" && \
           test "$enable_dlopen" != "no"],
          [AC_MSG_WARN([Did not find a suitable static prte prtedl component])
           AC_MSG_WARN([You might need to install libltld (and its headers) or])
           AC_MSG_WARN([specify --disable-dlopen to configure.])
           AC_MSG_ERROR([Cannot continue])])

    # If we have a winning component (which, per above, will only
    # happen if --disable-dlopen was *not* specified), do some more
    # logic.
    AS_IF([test "$MCA_prte_prtedl_STATIC_COMPONENTS" != ""],
       [ # We had a winner -- w00t!

        PRTE_HAVE_DL_SUPPORT=1
        # If we added any -L flags to ADD_LDFLAGS, then we (might)
        # need to add those directories to LD_LIBRARY_PATH.
        # Otherwise, if we try to AC RUN_IFELSE anything here in
        # configure, it might die because it can't find the libraries
        # we just linked against.
        PRTE_VAR_SCOPE_PUSH([prte_prtedl_base_found_l prte_prtedl_base_token prte_prtedl_base_tmp prte_prtedl_base_dir])
        prte_prtedl_base_found_l=0
        eval "prte_prtedl_base_tmp=\$prte_prtedl_${prte_prtedl_winner}_ADD_LIBS"
        for prte_prtedl_base_token in $prte_prtedl_base_tmp; do
            case $prte_prtedl_base_token in
            -l*) prte_prtedl_base_found_l=1 ;;
            esac
        done
        AS_IF([test $prte_prtedl_base_found_l -eq 1],
              [eval "prte_prtedl_base_tmp=\$prte_prtedl_${prte_prtedl_winner}_ADD_LDFLAGS"
               for prte_prtedl_base_token in $prte_prtedl_base_tmp; do
                   case $prte_prtedl_base_token in
                   -L*)
                       prte_prtedl_base_dir=`echo $prte_prtedl_base_token | cut -c3-`
                       export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$prte_prtedl_base_dir
                       AC_MSG_WARN([Adding to LD_LIBRARY_PATH: $prte_prtedl_base_dir])
                       ;;
                   esac
               done])
        PRTE_VAR_SCOPE_POP
    ])

    AC_DEFINE_UNQUOTED([PRTE_HAVE_DL_SUPPORT], [$PRTE_HAVE_DL_SUPPORT],
                       [Whether the PRTE DL framework is functional or not])
])
