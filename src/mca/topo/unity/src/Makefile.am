# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

headers = \
    topo_unity.h

noinst_LTLIBRARIES = libmca_topo_unity.la
libmca_topo_unity_la_SOURCES = \
    $(headers) \
    topo_unity_cart_map.c \
    topo_unity_graph_map.c \
    topo_unity.c \
    topo_unity_component.c
