#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_llm_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_llm_ALL_SUBDIRS)

# Source code files

headers = llm.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/llm
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
