#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
	pcm_rms.h \
	pcm_rms.c \
	pcm_rms_component.c \
	pcm_rms_job_list.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_pcm_rms_DSO
component_noinst =
component_install = mca_pcm_rms.la
else
component_noinst = libmca_pcm_rms.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_pcm_rms_la_SOURCES = $(sources)
mca_pcm_rms_la_LIBADD = 
mca_pcm_rms_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_pcm_rms_la_SOURCES = $(sources)
libmca_pcm_rms_la_LIBADD = 
libmca_pcm_rms_la_LDFLAGS = -module -avoid-version
