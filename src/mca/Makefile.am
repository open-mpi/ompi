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
	llm \
        mpool \
        ns \
        one \
        oob \
        pcm \
        pml \
        ptl \
        topo
# JMS Temporary hack to make "make dist" function properly; directories 
# mentioned here will eventually be moved into SUBDIRS
DIST_SUBDIRS = $(SUBDIRS) gpr

# Source code files

headers = mca.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
