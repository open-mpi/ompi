# -*- shell-script -*-
#
# Copyright (c) 2012-2015 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2015      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If CUDA support was requested, then build the CUDA memory pools.
# This code checks the variable CUDA_SUPPORT which was set earlier in
# the configure sequence by the opal_configure_options.m4 code.
#

AC_DEFUN([MCA_opal_rcache_rgpusm_CONFIG],[
    AC_CONFIG_FILES([opal/mca/rcache/rgpusm/Makefile])

    OPAL_CHECK_CUDA([rcache_rgpusm])

    # Use CUDA_SUPPORT which was filled in by the opal configure code.
    AS_IF([test "x$CUDA_SUPPORT" = "x1"],
          [$1],
          [$2])

    AC_SUBST([rcache_rgpusm_CPPFLAGS])
    AC_SUBST([rcache_rgpusm_LDFLAGS])
    AC_SUBST([rcache_rgpusm_LIBS])
])dnl
