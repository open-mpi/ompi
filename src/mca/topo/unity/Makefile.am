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

if OMPI_BUILD_topo_unity_DSO
component_noinst =
component_install = mca_topo_unity.la
else
component_noinst = libmca_topo_unity.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_topo_unity_la_SOURCES =
mca_topo_unity_la_LIBADD = src/libmca_topo_unity.la $(LIBMPI_LA)
mca_topo_unity_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_topo_unity_la_SOURCES =
libmca_topo_unity_la_LIBADD = src/libmca_topo_unity.la
libmca_topo_unity_la_LDFLAGS = -module -avoid-version

