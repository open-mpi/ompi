#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = \
        base \
        common_lam \
        common_mpi \
        coll \
        io \
        one \
        oob \
        pcm \
        pml \
        ptl \
        registry \
        topo

# Source code files

headers = mca.h

# Library

noinst_LTLIBRARIES = libmca.la

libmca_la_SOURCES =
libmca_la_LIBADD = $(LIBLTDL_LTLIB) \
        base/libmca_base.la \
        common/libmca_common.la \
        coll/libmca_coll.la \
        io/libmca_io.la \
        one/libmca_one.la \
        oob/libmca_oob.la \
        pcm/libmca_pcm.la \
        pml/libmca_pml.la \
        ptl/libmca_ptl.la \
        registry/libmca_registry.la \
        topo/libmca_topo.la

libmca_la_DEPENDENCIES = $(libmca_lam_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
