#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

sources = \
	pcm_rsh.h \
	pcm_rsh_allocate.c \
	pcm_rsh_component.c \
	pcm_rsh_kill.c \
	pcm_rsh_spawn.c


if OMPI_BUILD_pcm_rsh_DSO
component_noinst =
component_install = mca_pcm_rsh.la
else
component_noinst = libmca_pcm_rsh.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pcm_rsh_la_SOURCES = $(sources)
mca_pcm_rsh_la_LIBADD =
mca_pcm_rsh_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pcm_rsh_la_SOURCES = $(sources)
libmca_pcm_rsh_la_LIBADD = 
libmca_pcm_rsh_la_LDFLAGS = -module -avoid-version
