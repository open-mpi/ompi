# -*- shell-script -*-
#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_sysinfo_linux_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_sysinfo_linux_CONFIG],[

   case "${host}" in
   i?86-*|x86_64*|ia64-*|powerpc-*|powerpc64-*|sparc*-*)
              AS_IF([test -r "/proc/cpuinfo"],
                     [sysinfo_linux_happy="yes"],
                     [sysinfo_linux_happy="no"])
        ;;
   *)
        sysinfo_linux_happy="no"
        ;;
   esac

    AS_IF([test "$sysinfo_linux_happy" = "yes"], 
          [$1], 
          [$2])
])
