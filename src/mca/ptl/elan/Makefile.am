#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = -I$(top_ompi_builddir)/src/include \
	-I$(top_ompi_srcdir)/src -I$(top_ompi_srcdir)/src/include  \
	-I/usr/lib/qsnet/elan4/include 

SUBDIRS = src tests

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ptl_elan_DSO
component_noinst =
component_install = mca_ptl_elan.la
else
component_noinst = libmca_ptl_elan.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ptl_elan_la_SOURCES = 
mca_ptl_elan_la_LIBADD = src/libmca_ptl_elan.la
mca_ptl_elan_la_LDFLAGS = -module -avoid-version 

noinst_LTLIBRARIES = $(component_noinst)
libmca_ptl_elan_la_SOURCES =
libmca_ptl_elan_la_LIBADD = src/libmca_ptl_elan.la
libmca_ptl_elan_la_LDFLAGS = -module -avoid-version 
