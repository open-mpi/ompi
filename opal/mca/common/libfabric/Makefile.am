# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# See opal/mca/common/sm/Makefile.am for an explanation of the
# different library targets in this file.

#
# Embedded libfabric
#

lib_LTLIBRARIES =
noinst_LTLIBRARIES =

# We have to override the CFLAGS that come in from above to remove
# --pedantic (see configure.m4 for details).
CFLAGS = $(opal_common_libfabric_embedded_CFLAGS)

# Due to what might be a bug in Automake, we need to remove stamp-h?
# files manually.  See
# http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19418.
DISTCLEANFILES = libfabric/stamp-h?

# Only build this embedded copy if we're not using an external
# libfabric.
if OPAL_COMMON_LIBFABRIC_BUILD_EMBEDDED

comp_inst = lib@OPAL_LIB_PREFIX@mca_common_libfabric.la
comp_noinst = lib@OPAL_LIB_PREFIX@mca_common_libfabric_noinst.la

if MCA_BUILD_opal_common_libfabric_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

cppflags = \
	$(opal_common_libfabric_embedded_CPPFLAGS) \
	-D_GNU_SOURCE \
	-DSYSCONFDIR=\"$(sysconfdir)\" \
	-DRDMADIR=\"/tmp\" \
	-DEXTDIR=\"$(pkglibdir)\" \
	-DENABLE_DEBUG=0 \
	'-DVERSION="embedded libfabric de8444c9771ab9022d952a27134e44e5d8a5d722"' \
	'-DPACKAGE="embedded libfabric de8444c9771ab9022d952a27134e44e5d8a5d722"'
libadd = $(opal_common_libfabric_embedded_LIBADD)

libfabric_core_headers = \
	libfabric/include/fi.h \
	libfabric/include/fi_enosys.h \
	libfabric/include/fi_indexer.h \
	libfabric/include/fi_list.h \
	libfabric/include/fi_rbuf.h \
	libfabric/include/prov.h \
	libfabric/include/linux/osd.h \
	libfabric/include/osx/osd.h \
	libfabric/include/rdma/fabric.h \
	libfabric/include/rdma/fi_atomic.h \
	libfabric/include/rdma/fi_cm.h \
	libfabric/include/rdma/fi_domain.h \
	libfabric/include/rdma/fi_endpoint.h \
	libfabric/include/rdma/fi_eq.h \
	libfabric/include/rdma/fi_errno.h \
	libfabric/include/rdma/fi_log.h \
	libfabric/include/rdma/fi_prov.h \
	libfabric/include/rdma/fi_rma.h \
	libfabric/include/rdma/fi_tagged.h \
	libfabric/include/rdma/fi_trigger.h

libfabric_core_sources = \
	libfabric/src/fabric.c \
	libfabric/src/fi_tostr.c \
	libfabric/src/common.c \
	libfabric/src/enosys.c \
	libfabric/src/log.c

libfabric_usnic_headers = \
	libfabric/prov/usnic/src/fi_ext_usnic.h \
	libfabric/prov/usnic/src/usdf.h \
	libfabric/prov/usnic/src/usdf_av.h \
	libfabric/prov/usnic/src/usdf_cm.h \
	libfabric/prov/usnic/src/usdf_cq.h \
	libfabric/prov/usnic/src/usdf_dgram.h \
	libfabric/prov/usnic/src/usdf_endpoint.h \
	libfabric/prov/usnic/src/usdf_msg.h \
	libfabric/prov/usnic/src/usdf_progress.h \
	libfabric/prov/usnic/src/usdf_rdm.h \
	libfabric/prov/usnic/src/usdf_rudp.h \
	libfabric/prov/usnic/src/usdf_timer.h \
	libfabric/prov/usnic/src/usnic_direct/cq_desc.h \
	libfabric/prov/usnic/src/usnic_direct/cq_enet_desc.h \
	libfabric/prov/usnic/src/usnic_direct/kcompat.h \
	libfabric/prov/usnic/src/usnic_direct/kcompat_priv.h \
	libfabric/prov/usnic/src/usnic_direct/libnl1_utils.h \
	libfabric/prov/usnic/src/usnic_direct/libnl3_utils.h \
	libfabric/prov/usnic/src/usnic_direct/libnl_utils.h \
	libfabric/prov/usnic/src/usnic_direct/linux/delay.h \
	libfabric/prov/usnic/src/usnic_direct/linux/slab.h \
	libfabric/prov/usnic/src/usnic_direct/linux_types.h \
	libfabric/prov/usnic/src/usnic_direct/rq_enet_desc.h \
	libfabric/prov/usnic/src/usnic_direct/usd.h \
	libfabric/prov/usnic/src/usnic_direct/usd_caps.h \
	libfabric/prov/usnic/src/usnic_direct/usd_dest.h \
	libfabric/prov/usnic/src/usnic_direct/usd_device.h \
	libfabric/prov/usnic/src/usnic_direct/usd_ib_cmd.h \
	libfabric/prov/usnic/src/usnic_direct/usd_ib_sysfs.h \
	libfabric/prov/usnic/src/usnic_direct/usd_post.h \
	libfabric/prov/usnic/src/usnic_direct/usd_queue.h \
	libfabric/prov/usnic/src/usnic_direct/usd_socket.h \
	libfabric/prov/usnic/src/usnic_direct/usd_time.h \
	libfabric/prov/usnic/src/usnic_direct/usd_util.h \
	libfabric/prov/usnic/src/usnic_direct/usd_vnic.h \
	libfabric/prov/usnic/src/usnic_direct/usnic_abi.h \
	libfabric/prov/usnic/src/usnic_direct/usnic_direct.h \
	libfabric/prov/usnic/src/usnic_direct/usnic_ib_abi.h \
	libfabric/prov/usnic/src/usnic_direct/usnic_ip_utils.h \
	libfabric/prov/usnic/src/usnic_direct/usnic_user_utils.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_cq.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_devcmd.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_dev.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_enet.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_resource.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_rq.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_stats.h \
	libfabric/prov/usnic/src/usnic_direct/vnic_wq.h \
	libfabric/prov/usnic/src/usnic_direct/wq_enet_desc.h

libfabric_usnic_sources = \
	libfabric/prov/usnic/src/usdf_av.c \
	libfabric/prov/usnic/src/usdf_cm.c \
	libfabric/prov/usnic/src/usdf_cq.c \
	libfabric/prov/usnic/src/usdf_dgram.c \
	libfabric/prov/usnic/src/usdf_domain.c \
	libfabric/prov/usnic/src/usdf_endpoint.c \
	libfabric/prov/usnic/src/usdf_ep_dgram.c \
	libfabric/prov/usnic/src/usdf_ep_msg.c \
	libfabric/prov/usnic/src/usdf_ep_rdm.c \
	libfabric/prov/usnic/src/usdf_eq.c \
	libfabric/prov/usnic/src/usdf_fabric.c \
	libfabric/prov/usnic/src/usdf_mem.c \
	libfabric/prov/usnic/src/usdf_msg.c \
	libfabric/prov/usnic/src/usdf_pep.c \
	libfabric/prov/usnic/src/usdf_progress.c \
	libfabric/prov/usnic/src/usdf_rdm.c \
	libfabric/prov/usnic/src/usdf_timer.c \
	libfabric/prov/usnic/src/usnic_direct/libnl_utils_common.c \
	libfabric/prov/usnic/src/usnic_direct/usd_caps.c \
	libfabric/prov/usnic/src/usnic_direct/usd_dest.c \
	libfabric/prov/usnic/src/usnic_direct/usd_device.c \
	libfabric/prov/usnic/src/usnic_direct/usd_event.c \
	libfabric/prov/usnic/src/usnic_direct/usd_ib_cmd.c \
	libfabric/prov/usnic/src/usnic_direct/usd_ib_sysfs.c \
	libfabric/prov/usnic/src/usnic_direct/usd_mem.c \
	libfabric/prov/usnic/src/usnic_direct/usd_poll.c \
	libfabric/prov/usnic/src/usnic_direct/usd_post.c \
	libfabric/prov/usnic/src/usnic_direct/usd_post_raw_normal.c \
	libfabric/prov/usnic/src/usnic_direct/usd_post_udp_normal.c \
	libfabric/prov/usnic/src/usnic_direct/usd_post_udp_pio.c \
	libfabric/prov/usnic/src/usnic_direct/usd_queues.c \
	libfabric/prov/usnic/src/usnic_direct/usd_socket.c \
	libfabric/prov/usnic/src/usnic_direct/usd_vnic.c \
	libfabric/prov/usnic/src/usnic_direct/usnic_ip_utils.c \
	libfabric/prov/usnic/src/usnic_direct/vnic_cq.c \
	libfabric/prov/usnic/src/usnic_direct/vnic_dev.c \
	libfabric/prov/usnic/src/usnic_direct/vnic_rq.c \
	libfabric/prov/usnic/src/usnic_direct/vnic_wq.c

libfabric_psm_headers = \
	libfabric/prov/psm/src/psm_am.h \
	libfabric/prov/psm/src/psmx.h

libfabric_psm_sources = \
	libfabric/prov/psm/src/psmx_init.c \
	libfabric/prov/psm/src/psmx_domain.c \
	libfabric/prov/psm/src/psmx_cq.c \
	libfabric/prov/psm/src/psmx_cntr.c \
	libfabric/prov/psm/src/psmx_av.c \
	libfabric/prov/psm/src/psmx_ep.c \
	libfabric/prov/psm/src/psmx_cm.c \
	libfabric/prov/psm/src/psmx_tagged.c \
	libfabric/prov/psm/src/psmx_msg.c \
	libfabric/prov/psm/src/psmx_msg2.c \
	libfabric/prov/psm/src/psmx_rma.c \
	libfabric/prov/psm/src/psmx_atomic.c \
	libfabric/prov/psm/src/psmx_am.c \
	libfabric/prov/psm/src/psmx_mr.c \
	libfabric/prov/psm/src/psmx_wait.c \
	libfabric/prov/psm/src/psmx_poll.c \
	libfabric/prov/psm/src/psmx_util.c

sources = $(libfabric_core_sources)
if OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_USNIC
sources += $(libfabric_usnic_sources)
endif OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_USNIC
if OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_PSM
sources += $(libfabric_psm_sources)
endif OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_PSM

headers = $(libfabric_core_headers)
if OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_USNIC
headers += $(libfabric_usnic_headers)
endif OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_USNIC
if OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_PSM
headers += $(libfabric_psm_headers)
endif OPAL_COMMON_LIBFABRIC_HAVE_PROVIDER_PSM

lib@OPAL_LIB_PREFIX@mca_common_libfabric_la_SOURCES = $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_libfabric_la_CPPFLAGS = $(cppflags)
lib@OPAL_LIB_PREFIX@mca_common_libfabric_la_LDFLAGS = \
        -version-info $(libmca_opal_common_libfabric_so_version)
lib@OPAL_LIB_PREFIX@mca_common_libfabric_la_LIBADD = $(libadd)

lib@OPAL_LIB_PREFIX@mca_common_libfabric_noinst_la_SOURCES = $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_libfabric_noinst_la_CPPFLAGS = $(cppflags)
lib@OPAL_LIB_PREFIX@mca_common_libfabric_noinst_la_LIBADD = $(libadd)

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically (case 2), described above).

# See Makefile.ompi-rules for an explanation of the "V" macros, below
V=0
OMPI_V_LN_SCOMP = $(ompi__v_LN_SCOMP_$V)
ompi__v_LN_SCOMP_ = $(ompi__v_LN_SCOMP_$AM_DEFAULT_VERBOSITY)
ompi__v_LN_SCOMP_0 = @echo "  LN_S    " `basename $(comp_inst)`;

all-local:
	$(OMPI_V_LN_SCOMP) if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi

endif OPAL_COMMON_LIBFABRIC_BUILD_EMBEDDED
