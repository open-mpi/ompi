# -*- makefile -*-
#
# $HEADER$
#

include $(top_lam_srcdir)/config/Makefile.options

headers = \
    topo_unity.h

noinst_LTLIBRARIES = libmca_topo_unity.la
libmca_topo_unity_la_SOURCES = \
    $(headers) \
    topo_unity.c \
    topo_unity_module.c \
    cart_map.c \
    graph_map.c

# Conditionally install the header files
if WANT_INSTALL_HEADERS
	lamdir = $(includedir)/lam/mca/mpi/topo/unity/src/
	lam_HEADERS = $(headers)
else
	lamdir = $(includedir)
endif
