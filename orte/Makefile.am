#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

include $(top_srcdir)/config/Makefile.options

SUBDIRS = \
        include \
        class \
	dps \
        mca \
        runtime \
        util \
        . \
        dynamic-mca \
        tools

# Build the main ORTE library

lib_LTLIBRARIES = liborte.la
liborte_la_SOURCES =
liborte_la_LIBADD = \
        class/libclass.la \
        dps/libdps.la \
        mca/errmgr/base/libmca_errmgr_base.la \
        mca/gpr/base/data_type_support/libmca_gpr_base_data_type.la \
        mca/gpr/base/pack_api_cmd/libmca_gpr_base_pack.la \
        mca/gpr/base/unpack_api_response/libmca_gpr_base_unpack.la \
        mca/gpr/base/libmca_gpr_base.la $(MCA_gpr_STATIC_LTLIBS) \
        mca/iof/base/libmca_iof_base.la $(MCA_iof_STATIC_LTLIBS) \
        mca/ns/base/libmca_ns_base.la $(MCA_ns_STATIC_LTLIBS) \
        mca/ns/base/data_type_support/libmca_ns_base_data_type.la \
        mca/oob/base/libmca_oob_base.la $(MCA_oob_STATIC_LTLIBS) \
        mca/pls/base/libmca_pls_base.la $(MCA_pls_STATIC_LTLIBS) \
        mca/ras/base/libmca_ras_base.la $(MCA_ras_STATIC_LTLIBS) \
        mca/rds/base/libmca_rds_base.la $(MCA_rds_STATIC_LTLIBS) \
        mca/rmaps/base/libmca_rmaps_base.la $(MCA_rmaps_STATIC_LTLIBS) \
        mca/rmgr/base/libmca_orte_rmgr_base.la $(MCA_rmgr_STATIC_LTLIBS) \
        mca/rmgr/base/data_type_support/libmca_rmgr_base_data_type.la \
        mca/rml/base/libmca_rml_base.la $(MCA_rml_STATIC_LTLIBS) \
        mca/schema/base/libmca_schema_base.la $(MCA_schema_STATIC_LTLIBS) \
        mca/soh/base/libmca_soh_base.la $(MCA_soh_STATIC_LTLIBS) \
        mca/soh/base/data_type_support/libmca_soh_base_data_type.la \
        runtime/libruntime.la \
        util/libompiutil.la
liborte_la_DEPENDENCIES = $(liborte_la_LIBADD)
