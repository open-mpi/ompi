#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = alpha amd64 ia32 ia64 powerpc sparc64

noinst_HEADERS = atomic.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/include/sys
ompi_HEADERS = $(noinst_HEADERS)
else
ompidir = $(includedir)
endif
