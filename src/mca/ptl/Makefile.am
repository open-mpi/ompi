#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_ptl_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_ptl_ALL_SUBDIRS)

# Source code files

headers = ptl.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/ptl
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
