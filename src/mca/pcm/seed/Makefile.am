#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

EXTRA_DIST = VERSION

sources = \
	pcm_seed.h \
	pcm_seed.c \
	pcm_seed_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pcm_seed_DSO
component_noinst =
component_install = mca_pcm_seed.la
else
component_noinst = libmca_pcm_seed.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pcm_seed_la_SOURCES = $(sources)
mca_pcm_seed_la_LIBADD =
mca_pcm_seed_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pcm_seed_la_SOURCES = $(sources)
libmca_pcm_seed_la_LIBADD =
libmca_pcm_seed_la_LDFLAGS = -module -avoid-version
