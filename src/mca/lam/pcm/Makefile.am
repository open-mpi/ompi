#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_pcm_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_pcm_ALL_SUBDIRS)

# Source code files

headers = pcm.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/pcm
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
