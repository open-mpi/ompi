#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_topo_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_topo_ALL_SUBDIRS)

# Source code files

headers = topo.h

noinst_LTLIBRARIES = libmca_topo.la
libmca_topo_la_SOURCES =
libmca_topo_la_LIBADD = \
	base/libmca_topo_base.la \
	$(MCA_topo_STATIC_LTLIBS)
	libmca_topo_la_DEPENDENCIES = $(libmca_topo_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/topo
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
