#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
    oob_tcp.c \
    oob_tcp.h \
    oob_tcp_hdr.h \
    oob_tcp_addr.h \
    oob_tcp_addr.c \
    oob_tcp_msg.c \
    oob_tcp_msg.h \
    oob_tcp_peer.c \
    oob_tcp_ping.c \
    oob_tcp_peer.h \
    oob_tcp_recv.c \
    oob_tcp_send.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_oob_tcp_DSO
component_noinst =
component_install = mca_oob_tcp.la
else
component_noinst = libmca_oob_tcp.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_oob_tcp_la_SOURCES = $(sources)
mca_oob_tcp_la_LIBADD = $(LIBOMPI_LA)
mca_oob_tcp_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_oob_tcp_la_SOURCES = $(sources)
libmca_oob_tcp_la_LDFLAGS = -module -avoid-version

