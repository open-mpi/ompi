#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
        allocator_bucket.c \
        allocator_bucket_alloc.c \
        allocator_bucket_alloc.h 

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_allocator_bucket_DSO
component_noinst =
component_install = mca_allocator_bucket.la
else
component_noinst = libmca_allocator_bucket.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_allocator_bucket_la_SOURCES = $(sources)
mca_allocator_bucket_la_LIBADD = $(LIBOMPI_LA)
mca_allocator_bucket_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_allocator_bucket_la_SOURCES = $(sources)
libmca_allocator_bucket_la_LDFLAGS = -module -avoid-version

