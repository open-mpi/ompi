# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS		= sn0

headers = \
	atomic.h \
	smp_dev_params.h \
	ulm_os.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/os/irix
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
