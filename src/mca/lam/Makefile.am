#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# base should be listed first because it should be built first
# (probably not for any technical rason, but it makes sense to build
# the base first).  common_lam must be second, however, because
# modules under there may be statically linked to other modules.

SUBDIRS = base common_lam oob pcm registry 

# Library

noinst_LTLIBRARIES = libmca_lam_convenience.la
libmca_lam_convenience_la_SOURCES =
libmca_lam_convenience_la_LIBADD = \
        base/libmca_lam_base.la \
        oob/libmca_oob.la \
        pcm/libmca_pcm.la \
        registry/libmca_registry.la
# Add base, common_lam, oob, pcm, registry as required
libmca_lam_convenience_la_DEPENDENCIES = $(libmca_lam_convenience_la_LIBADD)
