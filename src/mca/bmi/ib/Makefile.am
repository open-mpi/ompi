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

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

libmca_bmi_ib_la_SOURCES = \
    bmi_ib.c \
    bmi_ib.h \
    bmi_ib_addr.h \
    bmi_ib_component.c \
    bmi_ib_endpoint.h \
    bmi_ib_frag.c \
    bmi_ib_frag.h \
    bmi_ib_memory.c \
    bmi_ib_peer.c \
    bmi_ib_peer.h \
    bmi_ib_priv.c \
    bmi_ib_priv.h \
    bmi_ib_proc.c \
    bmi_ib_proc.h \
    bmi_ib_recvfrag.c \
    bmi_ib_recvfrag.h \
    bmi_ib_sendfrag.c \
    bmi_ib_sendfrag.h \
    bmi_ib_vapi.h
    
     
# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_bmi_sm_DSO
component_noinst =
component_install = mca_bmi_ib.la
else
component_noinst = libmca_bmi_ib.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_bmi_ib_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_bmi_ib_la_LDFLAGS = -module -avoid-version
