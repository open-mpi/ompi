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

if OMPI_BUILD_ptl_sm_DSO
component_noinst =
component_install = mca_ptl_sm.la
else
component_noinst = libmca_ptl_sm.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_sm_la_SOURCES = 
mca_ptl_sm_la_LIBADD = src/libmca_ptl_sm.la
mca_ptl_sm_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_sm_la_SOURCES =
libmca_ptl_sm_la_LIBADD = src/libmca_ptl_sm.la
libmca_ptl_sm_la_LDFLAGS = -module -avoid-version
