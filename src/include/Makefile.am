#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

headers = \
        atomic.h \
        constants.h \
        ompi.h \
        totalview.h \
        types.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/include
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
