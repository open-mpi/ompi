#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_HEADERS = atomic.h atomic.s

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/include/sys/powerpc
ompi_HEADERS = $(noinst_HEADERS)
else
ompidir = $(includedir)
endif
