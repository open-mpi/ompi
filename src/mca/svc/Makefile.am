#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_svc_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_svc_ALL_SUBDIRS)

# Source code files

headers = svc.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/svc
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
