#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_pcm_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_pcm_ALL_SUBDIRS)

noinst_LTLIBRARIES = libmca_pcm.la

# Source code files

headers = pcm.h

# Library

libmca_pcm_la_SOURCES = $(headers)
libmca_pcm_la_LIBADD = \
        $(MCA_pcm_STATIC_LTLIBS) \
        base/libmca_pcm_base.la

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/pcm
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
