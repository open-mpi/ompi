#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = \
	allocator \
        base \
        coll \
        common \
        gpr \
        io \
	llm \
        mpool \
        ns \
        one \
        oob \
        pcm \
	pcmclient \
        pml \
        ptl \
        svc \
        topo

# Source code files

headers = mca.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
