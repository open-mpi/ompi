#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_coll_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_coll_ALL_SUBDIRS)

# Source code files

headers = coll.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/coll
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
