#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_registry_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_registry_ALL_SUBDIRS)

noinst_LTLIBRARIES = libmca_registry.la

# Source code files

headers = registry.h

# Library

libmca_registry_la_SOURCES = $(headers)
libmca_registry_la_LIBADD = \
        $(MCA_registry_STATIC_LTLIBS) \
        base/libmca_registry_base.la
libmca_registry_la_DEPENDENCIES = $(libmca_registry_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/registry
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
