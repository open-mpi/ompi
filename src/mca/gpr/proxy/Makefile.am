#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
        gpr_proxy.c \
        gpr_proxy.h \
        gpr_proxy_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_gpr_proxy_DSO
component_noinst =
component_install = mca_gpr_proxy.la
else
component_noinst = libmca_gpr_proxy.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_gpr_proxy_la_SOURCES = $(sources)
mca_gpr_proxy_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_gpr_proxy_la_SOURCES =$(sources)
libmca_gpr_proxy_la_LDFLAGS = -module -avoid-version
