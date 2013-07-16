# -*- shell-script -*-
#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_ompi_pubsub_pmi_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_pubsub_pmi_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/pubsub/pmi/Makefile])
         
    OPAL_CHECK_PMI([pubsub_pmi], [pubsub_pmi_good=1], [pubsub_pmi_good=0])
         
    # Evaluate succeed / fail
    AS_IF([test "$pubsub_pmi_good" = 1],
          [$1],
          [$2])

    # set build flags to use in makefile
    AC_SUBST([pubsub_pmi_CPPFLAGS])
    AC_SUBST([pubsub_pmi_LDFLAGS])
    AC_SUBST([pubsub_pmi_LIBS])

])
