# -*- shell-script -*-
#
# Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# ORCA_CHECK_ORTE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if ORTE support can be found.  sets:
#  prefix_{CFLAGS, CPPFLAGS, LDFLAGS, LIBS}
#  as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
#
# EXAMPLE CONFIGURE USAGE:
# 
AC_DEFUN([ORCA_CHECK_ORTE], [

    orca_check_orte_happy="yes"
    orca_check_orte_done="no"

    orca_check_orte_enabled_autogen="no"
    orca_check_orte_enabled_configure="no"

    #
    # Default build
    #  - Assume that ORTE is being built alongside opal and orca in tree
    #
    # If the user passed: ./autogen.pl --no-orte
    #  - Search for orte in library paths supplied
    #
    # If the user passed: ./configure --without-orte
    #  - Disable ORTE support
    #
    # if the user did: ./autogen.pl && ./configure --with-orte
    #  - Error out since we will be building ORTE in the tree, and not
    #    with the external build of ORTE. This is probably not what
    #    the user wanted to do.
    #
    # if the user did: ./autogen.pl && ./configure --without-orte
    #  - Warn since we will be building ORTE in the tree, and not
    #    without ORTE. Though this should be fine, it is doing more work
    #    than is necessary, and may not be what the user expects.
    #  

    #
    # Check for: ./autogen.pl --no-orte
    #
    m4_ifdef([project_orte],
        [orca_check_orte_enabled_autogen="yes"],
        [orca_check_orte_enabled_autogen="no"])

    #
    # Check for:
    #   ./configure --with-orte --with-orte-libs
    #   ./configure --without-orte
    #
    # /include
    #AC_ARG_WITH([orte],
    #    [AC_HELP_STRING([--with-orte(=DIR)],
    #            [Path to External ORTE Installation in DIR])])
    # /lib
    #AC_ARG_WITH([orte-libdir],
    #    [AC_HELP_STRING([--with-orte-libdir=DIR],
    #            [Search for External ORTE libraries in DIR])])

    #
    # JJH: Disable the --with-orte for now until we sort out the linking issues
    #
    m4_ifdef([project_orte],
        [orca_check_orte_enabled_autogen="yes"],
        [orca_check_orte_enabled_autogen="no"
         with_orte="no"])

    AS_IF([test ! -z "$with_orte" -a "$with_orte" != "no"],
        [orca_check_orte_enabled_configure="yes"],
        [AS_IF([test ! -z "$with_orte_libdir" -a "$with_orte_libdir" != "no"],
               [orca_check_orte_enabled_configure="yes"],
               [orca_check_orte_enabled_configure="no"]
               )
        ]
    )

    #
    # Case: Default
    #
    AC_MSG_CHECKING([if building orte as an inside project])
    AS_IF([test "$orca_check_orte_enabled_autogen" = "yes"],
        [
         #$1_CFLAGS="$CFLAGS -I$top_ompi_srcdir/orte"
         #$1_CPPFLAGS="$CPPFLAGS -I$top_ompi_srcdir/orte"
         $1_CFLAGS="$CFLAGS"
         $1_CPPFLAGS="$CPPFLAGS"
         $1_LDFLAGS="$LDFLAGS -L$top_ompi_srcdir/orte/"
         $1_LIBS="-lopen-rte $LIBS"
         orca_check_orte_done="yes"
         orca_check_orte_happy="yes"
        ])
    AC_MSG_RESULT([$orca_check_orte_done])

    #
    # Error and Warning Cases
    #
    AS_IF([test "$orca_check_orte_enabled_autogen" = "yes"],
        [
         # Error Case: ./autogen.pl && ./configure --with-orte
         AC_MSG_CHECKING([if passed --with-orte or --with-orte-libs])
         AS_IF([test "$orca_check_orte_enabled_configure" = "yes" ],
               [
                AC_MSG_RESULT([yes])
                AC_MSG_WARN([**************************************************])
                AC_MSG_WARN([*** ORTE was enabled in autogen, and asked for   *])
                AC_MSG_WARN([*** in configure (using either the --with-orte   *])
                AC_MSG_WARN([*** or --with-orte-libdir option). There is a    *])
                AC_MSG_WARN([*** potential conflict since ORTE will be built  *])
                AC_MSG_WARN([*** in the source tree, and override any options *])
                AC_MSG_WARN([*** supplied to configure.                       *])
                AC_MSG_WARN([*** If you intended to link against an external  *])
                AC_MSG_WARN([*** ORTE then rerun autogen with the --no-orte   *])
                AC_MSG_WARN([*** option.                                      *])
                AC_MSG_WARN([**************************************************])
                AC_MSG_ERROR([Will Not Continue])
                orca_check_orte_happy="no"
                orca_check_orte_done="yes"
               ],
               [AC_MSG_RESULT([no])])

         # Warning Case: ./autogen.pl && ./configure --without-orte
         AC_MSG_CHECKING([if passed --without-orte])
         AS_IF([test "$with_orte" == "no"],
               [
                AC_MSG_RESULT([yes])
                AC_MSG_WARN([**************************************************])
                AC_MSG_WARN([*** ORTE was enabled in autogen, and configure   *])
                AC_MSG_WARN([*** was passed --without-orte. ORTE will still   *])
                AC_MSG_WARN([*** be built and installed from the source tree. *])
                AC_MSG_WARN([*** If you intended to create a build without    *])
                AC_MSG_WARN([*** ORTE then you will need to rerun autogen.pl  *])
                AC_MSG_WARN([*** with the -no-orte option.                    *])
                AC_MSG_WARN([**************************************************])
                AC_MSG_ERROR([Will Not Continue])
                orca_check_orte_happy="yes"
                orca_check_orte_done="no"
               ],
               [AC_MSG_RESULT([no])])
         ]
        )

    #
    # Cases: ./autogen.pl --no-orte && ./configure
    #
    AS_IF([test "$orca_check_orte_done" = "no" ],
        [AC_MSG_CHECKING([if vanilla configure])
         AS_IF([test "$orca_check_orte_enabled_configure" = "no" ],
             [AC_MSG_RESULT([yes])
              orca_check_orte_happy="no"],
             [AC_MSG_RESULT([no])])
         ])

    #
    # Case: --no-orte to autogen.pl
    # Subcases:
    #   --with-orte      : ORTE installed in default search path
    #   --with-orte=PATH : ORTE installed at PATH
    #   --without-orte   : ORTE should not be built
    #
    AS_IF([test "$orca_check_orte_done" = "no" -a "$orca_check_orte_enabled_configure" = "yes" ],
        [
         #
         # Check if the library is usable
         #
         OMPI_CHECK_WITHDIR([orte], [$with_orte], [include/openmpi/orte/types.h])
         OMPI_CHECK_WITHDIR([orte-libdir], [$with_orte_libdir], [libopen-rte.*])

         AS_IF([test "$with_orte" = "no" -o "$orca_check_orte_happy" = "no"],
             [orca_check_orte_happy="no"],
             [orca_check_orte_happy="yes"])

         AS_IF([test "$orca_check_orte_happy" = "yes"],
             [AC_MSG_WARN([**************************************************])
              AC_MSG_WARN([*** You have chosen to try to link against a     *])
              AC_MSG_WARN([*** build of ORTE that is external to the source *])
              AC_MSG_WARN([*** tree. Support for this option is experimental*])
              AC_MSG_WARN([*** and should be used with care.                *])
              AC_MSG_WARN([**************************************************])
              ])

         orca_check_orte_dir_msg="compiler default"
         orca_check_orte_libdir_msg="linker default"
         orca_check_orte_dir=""
         orca_check_orte_libdir=""

         AS_IF([test "$orca_check_orte_happy" = "yes"],
             [AS_IF([test ! -z "$with_orte" -a "$with_orte" != "yes"],
                     [orca_check_orte_dir="$with_orte"
                      orca_check_orte_dir_msg="$with_orte (from --with-orte)"])
                 AS_IF([test ! -z "$with_orte_libdir" -a "$with_orte_libdir" != "yes"],
                     [orca_check_orte_libdir="$with_orte_libdir"
                      orca_check_orte_libdir_msg="$with_orte_libdir (from --with-orte-libdir)"])
                 ])

         orca_check_orte_$1_save_CPPFLAGS="$CPPFLAGS"
         CPPFLAGS="$CPPFLAGS -I$orca_check_orte_dir/include/openmpi"
         AS_IF([test "$orca_check_orte_happy" = "yes"],
             [AC_MSG_CHECKING([for ORTE dir])
              AC_MSG_RESULT([$orca_check_orte_dir_msg])
              AC_MSG_CHECKING([for ORTE library dir])
              AC_MSG_RESULT([$orca_check_orte_libdir_msg])
              OMPI_CHECK_PACKAGE([$1],
                  [openmpi/orte/runtime/runtime.h],
                  [open-rte],
                  [orte_finalize],
                  [],
                  [$orca_check_orte_dir],
                  [$orca_check_orte_libdir],
                  [orca_check_orte_happy="yes"],
                  [orca_check_orte_happy="no"])
              ])

         CPPFLAGS="$orca_check_orte_$1_save_CPPFLAGS"
         AS_IF([test "$orca_check_orte_happy" = "yes"],
             [orca_check_orte_done="yes"]
         )

         AS_IF([test "$orca_check_orte_happy" = "no"],
             AS_IF([test ! -z "$with_orte" -a "$with_orte" != "no"],
             [AC_MSG_WARN([ORTE support requested but not found. Perhaps you need to specify the location of the ORTE libraries.])
              AC_MSG_ERROR([Aborting.])
              $2]
             )
         )
    ])

    #
    # Now setup the additions to the wrapper compiler.  If '-lopen_rte' or
    # '-I' or '-L' was needed to link to ORTE, then OMPI_CHECK_PACKAGE
    # sets the orca_check_orte_check_* variables, which we use below.
    #
    AS_IF([test "$orca_check_orte_happy" = "yes"],
          [$2],
          [$3]);

    #AC_MSG_ERROR([Stopping. Happy "$orca_check_orte_happy"])
])

