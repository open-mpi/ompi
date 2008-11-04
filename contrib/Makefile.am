#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

amca_paramdir = $(AMCA_PARAM_SETS_DIR)
dist_amca_param_DATA = amca-param-sets/example.conf

if WANT_FT
dist_amca_param_DATA += amca-param-sets/ft-enable-cr
endif

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
        platform/lanl/macosx-dynamic \
        platform/lanl/macosx-dynamic-optimized \
        platform/lanl/macosx-static-leopard \
        platform/rr-class/debug-nopanasas \
        platform/rr-class/debug-panasas \
        platform/rr-class/debug.conf \
        platform/rr-class/debug-nopanasas.conf \
        platform/rr-class/debug-panasas.conf \
        platform/rr-class/optimized-nopanasas \
        platform/rr-class/optimized-panasas \
        platform/rr-class/optimized.conf \
        platform/rr-class/optimized-nopanasas.conf \
        platform/rr-class/optimized-panasas.conf \
        platform/tlcc/debug-nopanasas \
        platform/tlcc/debug-panasas \
        platform/tlcc/debug.conf \
        platform/tlcc/debug-nopanasas.conf \
        platform/tlcc/debug-panasas.conf \
        platform/tlcc/optimized-nopanasas \
        platform/tlcc/optimized-panasas \
        platform/tlcc/optimized.conf \
        platform/tlcc/optimized-nopanasas.conf \
        platform/tlcc/optimized-panasas.conf
