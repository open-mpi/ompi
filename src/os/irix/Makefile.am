# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = sn0

headers = \
	atomic.h \
	smp_dev_params.h \
	ulm_os.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/os/irix
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
