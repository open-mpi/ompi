# -*- shell-script -*-
#
# Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_flux_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_flux_CONFIG], [

    AC_CONFIG_FILES([opal/mca/pmix/flux/Makefile])

    AC_ARG_WITH([flux-pmi],
                [AC_HELP_STRING([--with-flux-pmi],
                                [Build Flux PMI support (default: yes)])])

    AC_ARG_WITH([flux-pmi-library],
                [AC_HELP_STRING([--with-flux-pmi-library],
                                [Link Flux PMI support with PMI library at build time.  Otherwise the library is opened at runtime at location specified by FLUX_PMI_LIBRARY_PATH environment variable.  Use this option to enable Flux support when building statically or without dlopen support (default: no)])])


    # pkg-config check aborts configure on failure
    AC_MSG_CHECKING([if user wants Flux support to link against PMI library])
    AS_IF([test "x$with_flux_pmi_library" != "xyes"],
          [AC_MSG_RESULT([no])
           $3],
          [AC_MSG_RESULT([yes])
          PKG_CHECK_MODULES([FLUX_PMI], [flux-pmi], [], [])
          have_flux_pmi_library=yes
          AC_DEFINE([HAVE_FLUX_PMI_LIBRARY], [1],
                    [Flux support builds against external PMI library])
          ])

    AC_MSG_CHECKING([if Flux support allowed to use dlopen])
    AS_IF([test $OPAL_ENABLE_DLOPEN_SUPPORT -eq 1 && test "x$compile_mode" = "xdso"],
          [AC_MSG_RESULT([yes])
          flux_can_dlopen=yes
          ],
          [AC_MSG_RESULT([no])
          ])

    AC_MSG_CHECKING([Checking if Flux PMI support can be built])
    AS_IF([test "x$with_flux_pmi" != "xno" && ( test "x$have_flux_pmi_library" = "xyes" || test "x$flux_can_dlopen" = "xyes" ) ],
          [AC_MSG_RESULT([yes])
          opal_enable_flux=yes
	  ],
          [AC_MSG_RESULT([no])
          AS_IF([test "x$with_flux_pmi" = "xyes"],
                [AC_MSG_ERROR([Aborting since Flux PMI support was requested])
                ])
          ])

    # Evaluate succeed / fail
    AS_IF([test "x$opal_enable_flux" = "xyes"],
          [$1
           # need to set the wrapper flags for static builds
           pmix_flux_WRAPPER_EXTRA_LIBS="$FLUX_PMI_LIBS"],
          [$2])
])
