#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_orte_iof_mr_hnp_DSO
component_noinst =
component_install = mca_iof_mr_hnp.la
else
component_noinst = libmca_iof_mr_hnp.la
component_install =
endif

mr_hnp_SOURCES = \
    iof_mrhnp.c \
    iof_mrhnp.h \
    iof_mrhnp_component.c \
    iof_mrhnp_read.c \
    iof_mrhnp_receive.c

mcacomponentdir = $(ompilibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_iof_mr_hnp_la_SOURCES = $(mr_hnp_SOURCES)
mca_iof_mr_hnp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_iof_mr_hnp_la_SOURCES = $(mr_hnp_SOURCES)
libmca_iof_mr_hnp_la_LIBADD =
libmca_iof_mr_hnp_la_LDFLAGS = -module -avoid-version
