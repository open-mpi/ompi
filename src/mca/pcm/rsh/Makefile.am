# -*- makefile -*-
#
# $HEADER$
#

# Use the top-level LAM Makefile.options

include $(top_lam_srcdir)/config/Makefile.options

SUBDIRS = src

EXTRA_DIST = VERSION

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# liblam_ssi_coll_lam_basic.la

if LAM_BUILD_pcm_rsh_LOADABLE_MODULE
module_noinst =
module_install = mca_pcm_rsh.la
else
module_noinst = libmca_lam_pcm_rsh.la
module_install =
endif

mcamoduledir = $(libdir)/lam
mcamodule_LTLIBRARIES = $(module_install)
mca_pcm_rsh_la_SOURCES =
mca_pcm_rsh_la_LIBADD = \
        src/libmca_pcm_rsh.la \
        $(LIBLAM_LA)
mca_pcm_rsh_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_lam_pcm_rsh_la_SOURCES =
libmca_lam_pcm_rsh_la_LIBADD = src/libmca_pcm_rsh.la
libmca_lam_pcm_rsh_la_LDFLAGS = -module -avoid-version
