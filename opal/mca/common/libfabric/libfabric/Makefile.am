AM_CPPFLAGS = \
	-I$(srcdir)/include \
	-D_GNU_SOURCE \
	-DSYSCONFDIR=\"$(sysconfdir)\" \
	-DRDMADIR=\"@rdmadir@\" \
	-DEXTDIR=\"$(pkglibdir)\"

lib_LTLIBRARIES = src/libfabric.la

pkglib_LTLIBRARIES = $(DL_PROVIDERS)

ACLOCAL_AMFLAGS = -I config
AM_CFLAGS = -g -Wall

if HAVE_LD_VERSION_SCRIPT
    libfabric_version_script = -Wl,--version-script=$(srcdir)/libfabric.map
else !HAVE_LD_VERSION_SCRIPT
    libfabric_version_script =
endif !HAVE_LD_VERSION_SCRIPT

# internal utility functions shared by in-tree providers:
common_srcs = \
	src/common.c \
	src/enosys.c

# ensure dl-built providers link back to libfabric
linkback = -lfabric -Lsrc/.libs/

src_libfabric_la_SOURCES = \
	include/fi.h \
	include/fi_enosys.h \
	include/fi_indexer.h \
	include/fi_list.h \
	include/fi_rbuf.h \
	src/fabric.c \
	src/fi_tostr.c \
	$(common_srcs)

if HAVE_SOCKETS
_sockets_files = \
	prov/sockets/src/sock.h \
	prov/sockets/src/sock_av.c \
	prov/sockets/src/sock_dgram.c \
	prov/sockets/src/sock_dom.c \
	prov/sockets/src/sock_eq.c \
	prov/sockets/src/sock_cq.c \
	prov/sockets/src/sock_cntr.c \
	prov/sockets/src/sock_poll.c \
	prov/sockets/src/sock_rdm.c \
	prov/sockets/src/sock_fabric.c \
	prov/sockets/src/sock_ep.c \
	prov/sockets/src/sock_ctx.c \
	prov/sockets/src/sock_util.c \
	prov/sockets/src/sock_util.h \
	prov/sockets/src/indexer.c \
	prov/sockets/src/list.c \
	prov/sockets/src/list.h

if HAVE_SOCKETS_DL
pkglib_LTLIBRARIES += libsockets-fi.la
libsockets_fi_la_SOURCES = $(_sockets_files) $(common_srcs)
libsockets_fi_la_LDFLAGS = -module -avoid-version -shared -export-dynamic $(linkback)
else !HAVE_SOCKETS_DL
src_libfabric_la_SOURCES += $(_sockets_files)
endif !HAVE_SOCKETS_DL

endif HAVE_SOCKETS

if HAVE_VERBS
_verbs_files = prov/verbs/src/fi_verbs.c

if HAVE_VERBS_DL
pkglib_LTLIBRARIES += libverbs-fi.la
libverbs_fi_la_SOURCES = $(_verbs_files) $(common_srcs)
libverbs_fi_la_LDFLAGS = -module -avoid-version -shared -export-dynamic -libverbs -lrdmacm $(linkback)
else !HAVE_VERBS_DL
src_libfabric_la_SOURCES += $(_verbs_files)
endif !HAVE_VERBS_DL

endif HAVE_VERBS

if HAVE_USNIC
libusnic_direct_sources = \
	prov/usnic/src/usnic_direct/cq_desc.h \
	prov/usnic/src/usnic_direct/cq_enet_desc.h \
	prov/usnic/src/usnic_direct/kcompat.h \
	prov/usnic/src/usnic_direct/kcompat_priv.h \
	prov/usnic/src/usnic_direct/libnl1_utils.h \
	prov/usnic/src/usnic_direct/libnl3_utils.h \
	prov/usnic/src/usnic_direct/libnl_utils_common.c \
	prov/usnic/src/usnic_direct/libnl_utils.h \
	prov/usnic/src/usnic_direct/linux/delay.h \
	prov/usnic/src/usnic_direct/linux/slab.h \
	prov/usnic/src/usnic_direct/linux_types.h \
	prov/usnic/src/usnic_direct/rq_enet_desc.h \
	prov/usnic/src/usnic_direct/usd_caps.c \
	prov/usnic/src/usnic_direct/usd_caps.h \
	prov/usnic/src/usnic_direct/usd_dest.c \
	prov/usnic/src/usnic_direct/usd_dest.h \
	prov/usnic/src/usnic_direct/usd_device.c \
	prov/usnic/src/usnic_direct/usd_device.h \
	prov/usnic/src/usnic_direct/usd_event.c \
	prov/usnic/src/usnic_direct/usd.h \
	prov/usnic/src/usnic_direct/usd_ib_cmd.c \
	prov/usnic/src/usnic_direct/usd_ib_cmd.h \
	prov/usnic/src/usnic_direct/usd_ib_sysfs.c \
	prov/usnic/src/usnic_direct/usd_ib_sysfs.h \
	prov/usnic/src/usnic_direct/usd_mem.c \
	prov/usnic/src/usnic_direct/usd_poll.c \
	prov/usnic/src/usnic_direct/usd_post.c \
	prov/usnic/src/usnic_direct/usd_post.h \
	prov/usnic/src/usnic_direct/usd_post_raw_normal.c \
	prov/usnic/src/usnic_direct/usd_post_udp_normal.c \
	prov/usnic/src/usnic_direct/usd_post_udp_pio.c \
	prov/usnic/src/usnic_direct/usd_queue.h \
	prov/usnic/src/usnic_direct/usd_queues.c \
	prov/usnic/src/usnic_direct/usd_socket.c \
	prov/usnic/src/usnic_direct/usd_socket.h \
	prov/usnic/src/usnic_direct/usd_time.h \
	prov/usnic/src/usnic_direct/usd_util.h \
	prov/usnic/src/usnic_direct/usd_vnic.c \
	prov/usnic/src/usnic_direct/usd_vnic.h \
	prov/usnic/src/usnic_direct/usnic_abi.h \
	prov/usnic/src/usnic_direct/usnic_direct.h \
	prov/usnic/src/usnic_direct/usnic_ib_abi.h \
	prov/usnic/src/usnic_direct/usnic_ip_utils.c \
	prov/usnic/src/usnic_direct/usnic_ip_utils.h \
	prov/usnic/src/usnic_direct/usnic_user_utils.h \
	prov/usnic/src/usnic_direct/vnic_cq.c \
	prov/usnic/src/usnic_direct/vnic_cq.h \
	prov/usnic/src/usnic_direct/vnic_dev.c \
	prov/usnic/src/usnic_direct/vnic_devcmd.h \
	prov/usnic/src/usnic_direct/vnic_dev.h \
	prov/usnic/src/usnic_direct/vnic_enet.h \
	prov/usnic/src/usnic_direct/vnic_resource.h \
	prov/usnic/src/usnic_direct/vnic_rq.c \
	prov/usnic/src/usnic_direct/vnic_rq.h \
	prov/usnic/src/usnic_direct/vnic_stats.h \
	prov/usnic/src/usnic_direct/vnic_wq.c \
	prov/usnic/src/usnic_direct/vnic_wq.h \
	prov/usnic/src/usnic_direct/wq_enet_desc.h

_usnic_files = \
	$(libusnic_direct_sources) \
	prov/usnic/src/fi_usnic.h \
	prov/usnic/src/usdf.h \
	prov/usnic/src/usdf_av.c \
	prov/usnic/src/usdf_av.h \
	prov/usnic/src/usdf_cm.c \
	prov/usnic/src/usdf_cm.h \
	prov/usnic/src/usdf_cq.c \
	prov/usnic/src/usdf_dgram.c \
	prov/usnic/src/usdf_dgram.h \
	prov/usnic/src/usdf_domain.c \
	prov/usnic/src/usdf_endpoint.c \
	prov/usnic/src/usdf_endpoint.h \
	prov/usnic/src/usdf_ep_dgram.c \
	prov/usnic/src/usdf_ep_msg.c \
	prov/usnic/src/usdf_eq.c \
	prov/usnic/src/usdf_fabric.c \
	prov/usnic/src/usdf_mem.c \
	prov/usnic/src/usdf_msg.c \
	prov/usnic/src/usdf_msg.h \
	prov/usnic/src/usdf_pep.c \
	prov/usnic/src/usdf_progress.c \
	prov/usnic/src/usdf_progress.h \
	prov/usnic/src/usdf_timer.c \
	prov/usnic/src/usdf_timer.h

_usnic_cppflags = \
        -D__LIBUSNIC__ \
        -I$(top_srcdir)/prov/usnic/src/usnic_direct

if HAVE_USNIC_DL
pkglib_LTLIBRARIES += libusnic-fi.la
libusnic_fi_la_CPPFLAGS = $(AM_CPPFLAGS) $(_usnic_cppflags)
libusnic_fi_la_SOURCES = $(_usnic_files) $(common_srcs)
libusnic_fi_la_LDFLAGS = -module -avoid-version -shared -export-dynamic
libusnic_fi_la_LIBS = $(linkback)
else !HAVE_USNIC_DL
AM_CPPFLAGS += $(_usnic_cppflags)
src_libfabric_la_SOURCES += $(_usnic_files)
endif !HAVE_USNIC_DL

endif HAVE_USNIC

if HAVE_PSM
_psm_files = \
	prov/psm/src/psmx.h \
	prov/psm/src/psmx_init.c \
	prov/psm/src/psmx_domain.c \
	prov/psm/src/psmx_cq.c \
	prov/psm/src/psmx_cntr.c \
	prov/psm/src/psmx_av.c \
	prov/psm/src/psmx_ep.c \
	prov/psm/src/psmx_cm.c \
	prov/psm/src/psmx_tagged.c \
	prov/psm/src/psmx_msg.c \
	prov/psm/src/psmx_msg2.c \
	prov/psm/src/psmx_rma.c \
	prov/psm/src/psmx_atomic.c \
	prov/psm/src/psmx_am.c \
	prov/psm/src/psmx_mr.c \
	prov/psm/src/psmx_wait.c \
	prov/psm/src/psmx_poll.c \
	prov/psm/src/psmx_util.c

if HAVE_PSM_DL
pkglib_LTLIBRARIES += libpsmx-fi.la
libpsmx_fi_la_SOURCES = $(_psm_files) $(common_srcs)
libpsmx_fi_la_LDFLAGS = -module -avoid-version -shared -export-dynamic $(linkback)
else !HAVE_PSM_DL
src_libfabric_la_SOURCES += $(_psm_files)
endif !HAVE_PSM_DL

endif HAVE_PSM

src_libfabric_la_LDFLAGS = -version-info 1 -export-dynamic \
			   $(libfabric_version_script)
src_libfabric_la_DEPENDENCIES = $(srcdir)/libfabric.map

rdmaincludedir = $(includedir)/rdma

rdmainclude_HEADERS = \
	$(top_srcdir)/include/rdma/fabric.h \
	$(top_srcdir)/include/rdma/fi_atomic.h \
	$(top_srcdir)/include/rdma/fi_cm.h \
	$(top_srcdir)/include/rdma/fi_domain.h \
	$(top_srcdir)/include/rdma/fi_eq.h \
	$(top_srcdir)/include/rdma/fi_prov.h \
	$(top_srcdir)/include/rdma/fi_rma.h \
	$(top_srcdir)/include/rdma/fi_endpoint.h \
	$(top_srcdir)/include/rdma/fi_errno.h \
	$(top_srcdir)/include/rdma/fi_tagged.h \
	$(top_srcdir)/include/rdma/fi_trigger.h

if HAVE_DIRECT
nodist_rdmainclude_HEADERS = \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_domain.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_endpoint.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_tagged.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_rma.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_atomic_def.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_atomic.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_cm.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_eq.h \
	$(top_srcdir)/prov/$(PROVIDER_DIRECT)/include/rdma/fi_direct_trigger.h
endif HAVE_DIRECT

man_MANS = \
	man/fabric.7 \
	man/fi_accept.3 \
	man/fi_alias.3 \
	man/fi_atomic.3 \
	man/fi_atomicv.3 \
	man/fi_atomicto.3 \
	man/fi_atomicmsg.3 \
	man/fi_atomic_valid.3 \
	man/fi_av.3 \
	man/fi_av_bind.3 \
	man/fi_av_insert.3 \
	man/fi_av_insertsvc.3 \
	man/fi_av_lookup.3 \
	man/fi_av_open.3 \
	man/fi_av_remove.3 \
	man/fi_av_straddr.3 \
	man/fi_cancel.3 \
	man/fi_close.3 \
	man/fi_cm.3 \
	man/fi_compare_atomic.3 \
	man/fi_compare_atomicv.3 \
	man/fi_compare_atomicto.3 \
	man/fi_compare_atomicmsg.3 \
	man/fi_compare_atomic_valid.3 \
	man/fi_connect.3 \
	man/fi_cntr.3 \
	man/fi_cntr_open.3 \
	man/fi_cntr_read.3 \
	man/fi_cntr_add.3 \
	man/fi_cntr_set.3 \
	man/fi_cntr_wait.3 \
	man/fi_cq.3 \
	man/fi_cq_open.3 \
	man/fi_cq_read.3 \
	man/fi_cq_readfrom.3 \
	man/fi_cq_readerr.3 \
	man/fi_cq_write.3 \
	man/fi_cq_sread.3 \
	man/fi_cq_sreadfrom.3 \
	man/fi_cq_strerror.3 \
	man/fi_direct.7 \
	man/fi_domain.3 \
	man/fi_domain_query.3 \
	man/fi_domain_bind.3 \
	man/fi_dupinfo.3 \
	man/fi_ep_bind.3 \
	man/fi_ep_sync.3 \
	man/fi_eq.3 \
	man/fi_eq_open.3 \
	man/fi_eq_read.3 \
	man/fi_eq_readerr.3 \
	man/fi_eq_write.3 \
	man/fi_eq_sread.3 \
	man/fi_eq_strerror.3 \
	man/fi_enable.3 \
	man/fi_endpoint.3 \
	man/fi_fabric.3 \
	man/fi_fetch_atomic.3 \
	man/fi_fetch_atomicv.3 \
	man/fi_fetch_atomicto.3 \
	man/fi_fetch_atomicmsg.3 \
	man/fi_fetch_atomic_valid.3 \
	man/fi_freeinfo.3 \
	man/fi_getinfo.3 \
	man/fi_getname.3 \
	man/fi_getopt.3 \
	man/fi_getpeer.3 \
	man/fi_inject.3 \
	man/fi_injectto.3 \
	man/fi_join.3 \
	man/fi_leave.3 \
	man/fi_listen.3 \
	man/fi_mr.3 \
	man/fi_mr_reg.3 \
	man/fi_mr_regv.3 \
	man/fi_mr_regattr.3 \
	man/fi_mr_desc.3 \
	man/fi_mr_key.3 \
	man/fi_mr_bind.3 \
	man/fi_msg.3 \
	man/fi_open.3 \
	man/fi_pendpoint.3 \
	man/fi_poll.3 \
	man/fi_poll_add.3 \
	man/fi_poll_del.3 \
	man/fi_poll_open.3 \
	man/fi_recv.3 \
	man/fi_recvv.3 \
	man/fi_recvfrom.3 \
	man/fi_recvmsg.3 \
	man/fi_reject.3 \
	man/fi_rma.3 \
	man/fi_rx_addr.3 \
	man/fi_send.3 \
	man/fi_senddata.3 \
	man/fi_senddatato.3 \
	man/fi_sendv.3 \
	man/fi_sendto.3 \
	man/fi_sendmsg.3 \
	man/fi_setopt.3 \
	man/fi_shutdown.3 \
	man/fi_tagged.3 \
	man/fi_tinject.3 \
	man/fi_tinjectto.3 \
	man/fi_tostr.3 \
	man/fi_trecv.3 \
	man/fi_trecvv.3 \
	man/fi_trecvfrom.3 \
	man/fi_trecvmsg.3 \
	man/fi_trigger.3 \
	man/fi_tsearch.3 \
	man/fi_tsend.3 \
	man/fi_tsenddata.3 \
	man/fi_tsenddatato.3 \
	man/fi_tsendv.3 \
	man/fi_tsendto.3 \
	man/fi_tsendmsg.3 \
	man/fi_version.3 \
	man/fi_wait.3 \
	man/fi_wait_open.3

EXTRA_DIST = libfabric.map libfabric.spec.in config/distscript.pl $(man_MANS)

dist-hook: libfabric.spec
	cp libfabric.spec $(distdir)
	"$(top_srcdir)/config/distscript.pl" "$(distdir)" "$(PACKAGE_VERSION)"
