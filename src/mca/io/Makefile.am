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
lamdir = $(includedir)/lam/mca/mpi/io
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
