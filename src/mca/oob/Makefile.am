#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_oob_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_oob_ALL_SUBDIRS)

noinst_LTLIBRARIES = libmca_oob.la

# Source code files

headers = oob.h

# Library

libmca_oob_la_SOURCES = $(headers)
libmca_oob_la_LIBADD = \
        $(MCA_oob_STATIC_LTLIBS) \
        base/libmca_oob_base.la
libmca_oob_la_DEPENDENCIES = $(libmca_oob_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/oob
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
