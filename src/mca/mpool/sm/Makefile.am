#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
	mpool_sm.h \
	mpool_sm_module.c \
	mpool_sm_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_mpool_sm_DSO
component_noinst =
component_install = mca_mpool_sm.la
else
component_noinst = libmca_mpool_sm.la
component_install =
endif

common = $(top_ompi_builddir)/src/mca/common/sm/libmca_common_sm.la

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_mpool_sm_la_SOURCES = $(sources)
mca_mpool_sm_la_LDFLAGS = -module -avoid-version
mca_mpool_sm_la_LIBADD = $(common)

noinst_LTLIBRARIES = $(component_noinst)
libmca_mpool_sm_la_SOURCES = $(sources)
libmca_mpool_sm_la_LDFLAGS = -module -avoid-version
libmca_mpool_sm_la_LIBADD = $(common)
