#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_one_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_one_ALL_SUBDIRS)

# Source code files

headers = one.h

# Conditonenally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/one
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
