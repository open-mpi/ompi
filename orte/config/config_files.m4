# -*- shell-script -*-
#
# Copyright (c) 2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This file is m4_included in the top-level configure.ac only if we
# are building the orte project.  You cannot put an AC_CONFIG_FILES in
# an AC_DEFUN that is conditionally called (because Autoconf will
# still process the AC_CONFIG_FILES unconditionally); you can only m4
# include the file or not.

AC_CONFIG_FILES([
    orte/Makefile
    orte/include/Makefile
    orte/etc/Makefile

    orte/tools/orted/Makefile
    orte/tools/orterun/Makefile
    orte/tools/wrappers/Makefile
    orte/tools/wrappers/ortecc-wrapper-data.txt
    orte/tools/wrappers/ortec++-wrapper-data.txt
    orte/tools/orte-checkpoint/Makefile
    orte/tools/orte-iof/Makefile
    orte/tools/orte-restart/Makefile
    orte/tools/orte-ps/Makefile
    orte/tools/orte-clean/Makefile
    orte/tools/orte-top/Makefile
    orte/tools/orte-bootproxy/Makefile
])
