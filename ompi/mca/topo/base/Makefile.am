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
        topo_base_find_available.c \
        topo_base_graph_create.c \
        topo_base_graphdims_get.c \
        topo_base_graph_get.c \
        topo_base_graph_neighbors.c \
        topo_base_graph_neighbors_count.c \
        topo_base_open.c \
        topo_base_close.c \
        topo_base_comm_select.c \
        topo_base_comm_unselect.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/ompi/mca/topo/base
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif


distclean-local:
	rm -f static-components.h
