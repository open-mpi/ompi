# -*- shell-script -*-
#
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([ORTE_CONFIG_FILES],[
    AC_CONFIG_FILES([
        orte/Makefile
        orte/include/Makefile
        orte/etc/Makefile
    
        orte/tools/orted/Makefile
        orte/tools/orterun/Makefile
        orte/tools/wrappers/Makefile
        orte/tools/wrappers/ortecc-wrapper-data.txt
        orte/tools/wrappers/orte.pc
        orte/tools/orte-checkpoint/Makefile
        orte/tools/orte-restart/Makefile
        orte/tools/orte-ps/Makefile
        orte/tools/orte-clean/Makefile
        orte/tools/orte-top/Makefile
        orte/tools/orte-migrate/Makefile
        orte/tools/orte-info/Makefile
        orte/tools/orte-server/Makefile
        orte/tools/orte-submit/Makefile
        orte/tools/orte-dvm/Makefile
    ])
])
