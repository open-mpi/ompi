#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_pcmclient_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_pcmclient_ALL_SUBDIRS)

# Source code files

headers = pcmclient.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/pcmclient
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
