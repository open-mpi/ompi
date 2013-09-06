# -*- shell-script -*-
#
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ompi_coll_hierarch_POST_CONFIG(will_build)
# ----------------------------------------
# The hierarch coll requires a BML endpoint tag to compile, so require it.
# Require in POST_CONFIG instead of CONFIG so that we only require it
# if we're not disabled.
AC_DEFUN([MCA_ompi_coll_hierarch_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([BML])])
])dnl

# MCA_ompi_coll_hierarch_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
# We can always build, unless we were explicitly disabled.
AC_DEFUN([MCA_ompi_coll_hierarch_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/hierarch/Makefile])
    [$1]
])dnl
