#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_oob_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_oob_ALL_SUBDIRS)


# Source code files

headers = oob.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/oob
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
