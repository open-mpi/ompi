#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = \
	allocator \
        base \
        coll \
        common \
        io \
        one \
        oob \
        pcm \
        pml \
        ptl \
        topo \
        registry

# Source code files

headers = mca.h

# Library

noinst_LTLIBRARIES = libmca.la

libmca_la_SOURCES =
libmca_la_LIBADD = \
        allocator/libmca_allocator.la \
        base/libmca_base.la \
        coll/libmca_coll.la \
        oob/libmca_oob.la \
        pcm/libmca_pcm.la \
        pml/libmca_pml.la \
        ptl/libmca_ptl.la \
        registry/libmca_registry.la
libmca_la_DEPENDENCIES = $(libmca_la_LIBADD)

not_implemented_yet = \
        one/libmca_one.la \
        io/libmca_io.la \
        common/libmca_common.la

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mca
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
