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

if OMPI_BUILD_coll_basic_DSO
component_noinst =
component_install = mca_coll_basic.la
else
component_noinst = libmca_coll_basic.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_coll_basic_la_SOURCES =
mca_coll_basic_la_LIBADD = src/libmca_coll_basic.la $(LIBMPI_LA)
mca_coll_basic_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_coll_basic_la_SOURCES =
libmca_coll_basic_la_LIBADD = src/libmca_coll_basic.la
libmca_coll_basic_la_LDFLAGS = -module -avoid-version
