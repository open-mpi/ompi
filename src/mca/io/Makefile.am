#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_io_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_io_ALL_SUBDIRS)

# Source code files

headers = io.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/io
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
