#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_topo_base.la

headers = \
         base.h

libmca_topo_base_la_SOURCES = \
        $(headers) \
        topo_base_cart_coords.c \
        topo_base_cart_create.c \
        topo_base_cartdim_get.c \
        topo_base_cart_get.c \
        topo_base_cart_rank.c \
        topo_base_cart_shift.c \
        topo_base_cart_sub.c \
        topo_base_graph_create.c \
        topo_base_graphdims_get.c \
        topo_base_graph_get.c \
        topo_base_graph_neighbors.c \
        topo_base_graph_neighbors_count.c \
        topo_base_open.c \
        topo_base_close.c \
        topo_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/topo/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

