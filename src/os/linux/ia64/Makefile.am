# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

headers = \
	atomic.h \
	lock.s \
	smp_dev_params.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/os/linux/ia64
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
