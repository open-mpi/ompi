#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

EXTRA_DIST = VERSION

sources = \
        pcmclient_seed.h \
        pcmclient_seed.c \
        pcmclient_seed_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pcmclient_seed_DSO
component_noinst =
component_install = mca_pcmclient_seed.la
else
component_noinst = libmca_pcmclient_seed.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pcmclient_seed_la_SOURCES = $(sources)
mca_pcmclient_seed_la_LIBADD =
mca_pcmclient_seed_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pcmclient_seed_la_SOURCES = $(sources)
libmca_pcmclient_seed_la_LIBADD =
libmca_pcmclient_seed_la_LDFLAGS = -module -avoid-version
