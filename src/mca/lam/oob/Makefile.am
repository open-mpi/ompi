#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_oob_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_oob_ALL_SUBDIRS)

# Source code files

headers = oob.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/oob
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
