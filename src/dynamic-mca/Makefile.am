#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = \
	allocator \
        coll \
        common \
        io \
        mpool \
	llm \
        ns \
        one \
        oob \
        pcm \
	pcmclient \
        pml \
        ptl \
        topo
# JMS Temporary hack to make "make dist" function properly; directories 
# mentioned here will eventually be moved into SUBDIRS
DIST_SUBDIRS = $(SUBDIRS) gpr
