#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_registry_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_registry_ALL_SUBDIRS)

# Source code files

headers = registry.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/registry
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
