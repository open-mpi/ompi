#
# Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# C bindings
#
headers = \
	mpiext_example_c.h

sources = \
	c/progress.c

#
# F77 bindings
#
f77_sources =
if OMPI_WANT_F77_BINDINGS
headers += \
	mpiext_example_f77.h

f77_sources += \
	f77/progress_f.c
endif

#
# F90 bindings
#
if OMPI_WANT_F77_BINDINGS
headers += \
	mpiext_example_f90.h
endif

extcomponentdir = $(pkglibdir)

# For C Bindings
lib = libext_mpiext_example.la
lib_sources = $(sources)

libext_mpiext_example_la_SOURCES = $(lib_sources)
libext_mpiext_example_la_LDFLAGS = -module -avoid-version

# For F77 Bindings
AM_CPPFLAGS = -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_F77_WRAPPERS=1
f77_lib = libext_mpiext_example_f77.la
f77_lib_sources = $(f77_sources)

libext_mpiext_example_f77_la_SOURCES = $(f77_lib_sources)
libext_mpiext_example_f77_la_LIBADD  = $(lib_sources)
libext_mpiext_example_f77_la_LDFLAGS = -module -avoid-version

# For F90 Bindings
#f90_lib = libext_mpiext_example_f90.la
#f90_lib_sources = 

#libext_mpiext_example_f90_la_SOURCES = $(f90_lib_sources)


noinst_LTLIBRARIES = $(lib) $(f77_lib)

ompidir = $(includedir)/openmpi/ompi/mpiext/example
ompi_HEADERS = \
        $(headers)
