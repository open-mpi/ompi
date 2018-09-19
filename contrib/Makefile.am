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
# Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      IBM Corporation.  All rights reserved.
# Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2013-2018 Los Alamos National Security, Inc.  All rights
#                         reserved.
# Copyright (c) 2013      Intel Corporation.  All rights reserved.
# Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
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
	dist/make-authors.pl \
	dist/linux/openmpi.spec \
	dist/mofed/compile_debian_mlnx_example.in \
	dist/mofed/debian \
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
        platform/lanl/cray_xc_cle5.2/debug-common \
        platform/lanl/cray_xc_cle5.2/debug-lustre \
        platform/lanl/cray_xc_cle5.2/debug-lustre.conf \
        platform/lanl/cray_xc_cle5.2/optimized-common \
        platform/lanl/cray_xc_cle5.2/optimized-lustre \
        platform/lanl/cray_xc_cle5.2/optimized-lustre.conf \
        platform/lanl/toss/README \
        platform/lanl/toss/common \
        platform/lanl/toss/common-optimized \
        platform/lanl/toss/cray-lustre-optimized \
        platform/lanl/toss/cray-lustre-optimized.conf \
        platform/lanl/toss/toss2-mlx-optimized \
        platform/lanl/toss/toss2-mlx-optimized.conf \
        platform/lanl/toss/toss2-qib-optimized \
        platform/lanl/toss/toss2-qib-optimized.conf \
        platform/lanl/toss/toss3-hfi-optimized \
        platform/lanl/toss/toss3-hfi-optimized.conf \
        platform/lanl/toss/toss3-mlx-optimized \
        platform/lanl/toss/toss3-mlx-optimized.conf \
        platform/lanl/toss/toss3-wc-optimized \
        platform/lanl/toss/toss3-wc-optimized.conf \
        platform/lanl/darwin/darwin-common \
        platform/lanl/darwin/debug-common \
        platform/lanl/darwin/optimized-common \
        platform/lanl/darwin/mic-common \
        platform/lanl/darwin/debug \
        platform/lanl/darwin/debug.conf \
        platform/lanl/darwin/optimized \
        platform/lanl/darwin/optimized.conf \
	platform/snl/portals4-m5 \
	platform/snl/portals4-orte \
	platform/ibm/debug-ppc32-gcc \
	platform/ibm/debug-ppc64-gcc \
	platform/ibm/optimized-ppc32-gcc \
	platform/ibm/optimized-ppc64-gcc \
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

dist_opaldata_DATA = openmpi-valgrind.supp
