#
# $HEADER$
#

# A word of explanation...
#
# This library is linked against various MCA components because all
# shared-memory based components (e.g., mpool, ptl, etc.)  need to
# share some common code and data.  There's two cases:
#
# 1. libmca_common_sm.la is a shared library.  By linking that shared
# library to all components that need it, the OS linker will
# automatically load it into the process as necessary, and there will
# only be one copy (i.e., all the components will share *one* copy of
# the code and data).
#
# 2. libmca_common_sm.la is a static library.  In this case, it will
# be rolled up into the top-level libmpi.la.  It will also be rolled
# into each component, but then the component will also be rolled up
# into the upper-level libmpi.la.  Linkers universally know how to
# "figure this out" so that we end up with only one copy of the code
# and data.

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
