#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_topo_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_topo_ALL_SUBDIRS)

# Source code files

headers = topo.h

# Condittoponally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/topo
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
