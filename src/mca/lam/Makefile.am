#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# base should be listed first because it should be built first
# (probably not for any technical rason, but it makes sense to build
# the base first).  common_lam must be second, however, because
# modules under there may be statically linked to other modules.

SUBDIRS = base common_lam oob pcm registry 

# Source code files

#headers = lam.h

# Library

noinst_LTLIBRARIES = libmca_lam_convenience.la
libmca_lam_convenience_la_SOURCES =
libmca_lam_convenience_la_LIBADD = \
        base/libmca_lam_base.la
# Add base, common_lam, oob, pcm, registry as required
libmca_lam_convenience_la_DEPENDENCIES = $(libmca_lam_convenience_la_LIBADD)

# Conditionally install the header files

#if WANT_INSTALL_HEADERS
#lamdir = $(includedir)/lam/mca/lam
#lam_HEADERS = $(headers)
#else
#lamdir = $(includedir)
#endif
