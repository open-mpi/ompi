#
# Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      IBM Corporation.  All rights reserved.
# Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

amca_paramdir = $(AMCA_PARAM_SETS_DIR)
dist_amca_param_DATA = amca-param-sets/example.conf

if WANT_FT_CR
dist_amca_param_DATA += \
	amca-param-sets/ft-enable-cr \
	amca-param-sets/ft-enable-cr-recovery
endif # WANT_FT_CR

EXTRA_DIST = \
	dist/make_dist_tarball \
	dist/linux/openmpi.spec \
	dist/macosx-pkg/buildpackage.sh \
	dist/macosx-pkg/ReadMe.rtf \
	platform/optimized \
	platform/redstorm \
	platform/cray_xt3 \
	platform/cray_xt3_romio \
	platform/ornl/cray_xt_cnl_romio \
	platform/ornl/cray_xt_cnl_romio_debug \
        platform/ornl/ornl_configure.gnu \
        platform/ornl/ornl_configure.pgi \
        platform/ornl/ornl_configure_self_contained \
        platform/lanl/macosx-dynamic \
        platform/lanl/macosx-dynamic.conf \
        platform/lanl/cray_xe6/debug-common \
        platform/lanl/cray_xe6/debug-lustre \
        platform/lanl/cray_xe6/debug-lustre.conf \
        platform/lanl/cray_xe6/optimized-common \
        platform/lanl/cray_xe6/optimized-lustre \
        platform/lanl/cray_xe6/optimized-lustre.conf \
        platform/lanl/rr-class/cross-debug-slave \
        platform/lanl/rr-class/cross-debug-slave.conf \
        platform/lanl/rr-class/cross-optimized-slave \
        platform/lanl/rr-class/cross-optimized-slave.conf \
        platform/lanl/rr-class/debug-nopanasas \
        platform/lanl/rr-class/debug-nopanasas.conf \
        platform/lanl/rr-class/debug-panasas \
        platform/lanl/rr-class/debug-panasas.conf \
        platform/lanl/rr-class/optimized-nopanasas \
        platform/lanl/rr-class/optimized-nopanasas.conf \
        platform/lanl/rr-class/optimized-panasas \
        platform/lanl/rr-class/optimized-panasas.conf \
        platform/lanl/rr-class/debug-master \
        platform/lanl/rr-class/debug-master.conf \
        platform/lanl/rr-class/optimized-master \
        platform/lanl/rr-class/optimized-master.conf \
        platform/lanl/rr-class/debug-panasas-nocell \
        platform/lanl/rr-class/debug-panasas-nocell.conf \
        platform/lanl/rr-class/debug-nopanasas-nocell \
        platform/lanl/rr-class/debug-nopanasas-nocell.conf \
        platform/lanl/rr-class/optimized-nopanasas-nocell \
        platform/lanl/rr-class/optimized-nopanasas-nocell.conf \
        platform/lanl/rr-class/optimized-panasas-nocell \
        platform/lanl/rr-class/optimized-panasas-nocell.conf \
        platform/lanl/tlcc/debug-nopanasas \
        platform/lanl/tlcc/debug-nopanasas.conf \
        platform/lanl/tlcc/debug-panasas \
        platform/lanl/tlcc/debug-panasas.conf \
        platform/lanl/tlcc/optimized-nopanasas \
        platform/lanl/tlcc/optimized-nopanasas.conf \
        platform/lanl/tlcc/optimized-panasas \
        platform/lanl/tlcc/optimized-panasas.conf \
        platform/lanl/tlcc2/debug-nopanasas \
        platform/lanl/tlcc2/debug-nopanasas.conf \
        platform/lanl/tlcc2/debug-panasas \
        platform/lanl/tlcc2/debug-panasas.conf \
        platform/lanl/tlcc2/optimized-nopanasas \
        platform/lanl/tlcc2/optimized-nopanasas.conf \
        platform/lanl/tlcc2/optimized-panasas \
        platform/lanl/tlcc2/optimized-panasas.conf \
	platform/snl/portals4-m5 \
	platform/snl/portals4-orte \
	platform/cisco/macosx-dynamic \
	platform/cisco/macosx-dynamic.conf \
	platform/cisco/linux \
	platform/cisco/linux.conf \
	platform/ibm/debug-ppc32-gcc \
	platform/ibm/debug-ppc64-gcc \
	platform/ibm/optimized-ppc32-gcc \
	platform/ibm/optimized-ppc64-gcc \
	platform/ibm/optimized-power7-gcc \
	platform/ibm/optimized-power6-gcc \
	platform/ibm/debug-power7-gcc \
	platform/ibm/debug-power6-gcc \
        platform/hadoop/linux \
        platform/hadoop/linux.conf \
        platform/hadoop/mac \
        platform/hadoop/mac.conf \
	platform/greenplum/bend/mac \
	platform/greenplum/bend/mac.conf \
	platform/greenplum/bend/linux \
	platform/greenplum/bend/linux.conf \
	platform/greenplum/bend/mac-optimized \
	platform/greenplum/bend/mac-optimized.conf \
	platform/greenplum/bend/linux-optimized \
	platform/greenplum/bend/linux-optimized.conf
	platform/greenplum/mrplus/mac \
	platform/greenplum/mrplus/mac.conf \
	platform/greenplum/mrplus/linux \
	platform/greenplum/mrplus/linux.conf \
	platform/greenplum/mrplus/mac-optimized \
	platform/greenplum/mrplus/mac-optimized.conf \
	platform/greenplum/mrplus/linux-optimized \
	platform/greenplum/mrplus/linux-optimized.conf

dist_pkgdata_DATA = openmpi-valgrind.supp
