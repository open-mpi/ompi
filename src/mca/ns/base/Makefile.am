#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

SUBDIRS = src

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_ns_base_DSO
component_noinst = 
component_install = mca_ns_base.la
else
component_noinst = libmca_ns_base.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_ns_base_la_SOURCES =
mca_ns_base_la_LIBADD = src/libmca_ns_base.la
mca_ns_base_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_ns_base_la_SOURCES =
libmca_ns_base_la_LIBADD = src/libmca_ns_base.la
libmca_ns_base_la_LDFLAGS = -module -avoid-version
