# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
        oob_tcp.c \
	oob_tcp_recv.c \
	oob_tcp_send.c \
	oob_tcp_peer.c

if OMPI_BUILD_allocator_bucket_LOADABLE_MODULE
module_noinst =
module_install = mca_oob_tcp.la
else
module_noinst = libmca_oob_tcp.la
module_install =
endif

mcamoduledir = $(libdir)/openmpi
mcamodule_LTLIBRARIES = $(module_install)
mca_oob_tcp_la_SOURCES = $(sources)
mca_oob_tcp_la_LIBADD = $(LIBOMPI_LA)
mca_oob_tcp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_oob_tcp_la_SOURCES = $(sources)
libmca_oob_tcp_la_LDFLAGS = -module -avoid-version

