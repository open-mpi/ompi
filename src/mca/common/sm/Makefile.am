#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

# Header files

headers = \
        common_sm_mmap.h

# Make the output library in this directory, and name it
# libmca_<type>_<name>.la.  This library needs to be installed.

lib_LTLIBRARIES = libmca_common_sm.la
libmca_common_sm_la_SOURCES =  \
        common_sm_mmap.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/common/sm
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
