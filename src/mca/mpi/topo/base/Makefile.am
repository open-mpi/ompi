#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpi_topo_base.la

headers = \
         base.h

libmca_mpi_coll_base_la_SOURCES = \
        $(headers) \
        topo_base_cart_coords.c \
        topo_base_cart_create.c \
        topo_base_cart_dim_get.c \
        topo_base_cart_get.c \
        topo_base_cart_rank.c \
        topo_base_cart_shift.c \
        topo_base_cart_sub.c \
        topo_base_graph_create.c \
        topo_base_graph_dims_get.c \
        topo_base_graph_get.c \
        topo_base_graph_neighbors.c \
        topo_base_graph_neighbors_count.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
	lamdir = $(includedir)/lam/mca/mpi/topo/base
	lam_HEADERS = $(headers)
else
	lamdir = $(includedir)
endif

