#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_mpool_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_mpool_ALL_SUBDIRS)

# Source code files

headers = mpool.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/mpool
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
