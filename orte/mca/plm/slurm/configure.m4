# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2015 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_plm_slurm_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_plm_slurm_CONFIG],[
    AC_CONFIG_FILES([orte/mca/plm/slurm/Makefile])

    ORTE_CHECK_SLURM([plm_slurm], [plm_slurm_good=1], [plm_slurm_good=0])

    # if check worked, set wrapper flags if so.
    # Evaluate succeed / fail
    AS_IF([test "$plm_slurm_good" = "1"],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([plm_slurm_CPPFLAGS])
    AC_SUBST([plm_slurm_LDFLAGS])
    AC_SUBST([plm_slurm_LIBS])

    # check to see if this is a Cray nativized slurm env.

    slurm_cray_env=0
    ORTE_CHECK_ALPS([plm_slurm_cray],
                    [slurm_cray_env=1])

    AC_DEFINE_UNQUOTED([SLURM_CRAY_ENV],[$slurm_cray_env],
                       [defined to 1 if slurm cray env, 0 otherwise])
])dnl
