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
# Copyright (c) 2013      Intel Corporation.  All rights reserved.
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
        completion/mpirun.sh \
        completion/mpirun.zsh \
	dist/make_dist_tarball \
	dist/linux/openmpi.spec \
	dist/mofed/compile_debian_mlnx_example.in \
	dist/mofed/debian \
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
        platform/lanl/cray_xe6/cray-common \
        platform/lanl/cray_xe6/debug-common \
        platform/lanl/cray_xe6/debug-lustre \
        platform/lanl/cray_xe6/debug-lustre.conf \
        platform/lanl/cray_xe6/gnu-common \
        platform/lanl/cray_xe6/intel-common \
        platform/lanl/cray_xe6/optimized-common \
        platform/lanl/cray_xe6/optimized-lustre \
        platform/lanl/cray_xe6/optimized-lustre.conf \
        platform/lanl/toss/debug-common \
        platform/lanl/toss/debug-nopanasas \
        platform/lanl/toss/debug-nopanasas.conf \
        platform/lanl/toss/debug-panasas \
        platform/lanl/toss/debug-panasas.conf \
        platform/lanl/toss/optimized-common \
        platform/lanl/toss/optimized-nopanasas \
        platform/lanl/toss/optimized-nopanasas.conf \
        platform/lanl/toss/optimized-panasas \
        platform/lanl/toss/optimized-panasas.conf \
        platform/lanl/toss/toss-common \
        platform/lanl/darwin/darwin-common \
        platform/lanl/darwin/debug-common \
        platform/lanl/darwin/optimized-common \
        platform/lanl/darwin/mic-common \
        platform/lanl/darwin/debug \
        platform/lanl/darwin/debug.conf \
        platform/lanl/darwin/debug-mic \
        platform/lanl/darwin/debug-mic.conf \
        platform/lanl/darwin/optimized \
        platform/lanl/darwin/optimized.conf \
        platform/lanl/darwin/optimized-mic \
        platform/lanl/darwin/optimized-mic.conf \
	platform/snl/portals4-m5 \
	platform/snl/portals4-orte \
	platform/ibm/debug-ppc32-gcc \
	platform/ibm/debug-ppc64-gcc \
	platform/ibm/optimized-ppc32-gcc \
	platform/ibm/optimized-ppc64-gcc \
	platform/ibm/optimized-power7-gcc \
	platform/ibm/optimized-power6-gcc \
	platform/ibm/debug-power7-gcc \
	platform/ibm/debug-power6-gcc \
	platform/intel/bend/mac \
	platform/intel/bend/mac.conf \
	platform/intel/bend/linux \
	platform/intel/bend/linux.conf \
	platform/intel/bend/mac-optimized \
	platform/intel/bend/mac-optimized.conf \
	platform/intel/bend/linux-optimized \
	platform/intel/bend/linux-optimized.conf \
	platform/mellanox/optimized \
	platform/mellanox/optimized.conf

dist_ompidata_DATA = openmpi-valgrind.supp
