#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

SUBDIRS = src 

EXTRA_DIST = VERSION

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ptl_ib_DSO
component_noinst =
component_install = mca_ptl_ib.la
else
component_noinst = libmca_ptl_ib.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_ib_la_SOURCES = 
mca_ptl_ib_la_LIBADD = src/libmca_ptl_ib.la
mca_ptl_ib_la_LDFLAGS = -module -avoid-version 

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_ib_la_SOURCES =
libmca_ptl_ib_la_LIBADD = src/libmca_ptl_ib.la
libmca_ptl_ib_la_LDFLAGS = -module -avoid-version 
