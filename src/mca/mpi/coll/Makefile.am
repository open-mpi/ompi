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
lamdir = $(includedir)/lam/mca/mpi/coll
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
