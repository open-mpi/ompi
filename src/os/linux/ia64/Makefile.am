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
ompidir = $(includedir)/ompi/os/linux/ia64
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
