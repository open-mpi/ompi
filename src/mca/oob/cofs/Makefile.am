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

if OMPI_BUILD_oob_cofs_DSO
component_noinst = 
component_install = mca_oob_cofs.la
else
component_noinst = libmca_oob_cofs.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_oob_cofs_la_SOURCES =
mca_oob_cofs_la_LIBADD = src/libmca_oob_cofs.la
mca_oob_cofs_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_oob_cofs_la_SOURCES =
libmca_oob_cofs_la_LIBADD = src/libmca_oob_cofs.la
libmca_oob_cofs_la_LDFLAGS = -module -avoid-version
