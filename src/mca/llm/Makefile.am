#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_llm_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_llm_ALL_SUBDIRS)

noinst_LTLIBRARIES = libmca_llm.la

# Source code files

headers = llm.h

# Library

libmca_llm_la_SOURCES = $(headers)
libmca_llm_la_LIBADD = \
        $(MCA_llm_STATIC_LTLIBS) \
        base/libmca_llm_base.la
libmca_llm_la_DEPENDENCIES = $(libmca_llm_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/llm
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
