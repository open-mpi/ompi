#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_topo_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_topo_ALL_SUBDIRS)

# Source code files

headers = topo.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mca/topo
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
