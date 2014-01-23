# -*- makefile -*-
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

noinst_LTLIBRARIES =

EXTRA_DIST = \
     attr_fn-f90-interfaces.h \
     conversion_fn_null-f90-interface.h

#-----------------------------------------------------------------------------

# If we're building any Fortran bindings, then we need to build the
# Fortran glue that will get slurped into libmpi
if OMPI_BUILD_FORTRAN_MPIFH_BINDINGS
noinst_LTLIBRARIES += libmpi_fortran_base.la

# This needs to be its own convenience library because it requires
# some specialized CPPFLAGS to set all the defines in various .h files
# properly (copied from ompi/mpi/fortran/mpif-h/Makefile.am) -- we
# unfortunately can't just suck the sources into the larger libmpi.la.

libmpi_fortran_base_la_CPPFLAGS = \
        -DOMPI_PROFILE_LAYER=0 -DOMPI_COMPILING_FORTRAN_WRAPPERS=1
libmpi_fortran_base_la_SOURCES = \
        constants.h \
        datarep.h \
        fint_2_int.h \
        strings.h \
        attr_fn_f.c \
        conversion_fn_null_f.c \
        f90_accessors.c \
        strings.c \
        test_constants_f.c
endif

#-----------------------------------------------------------------------------

# If we're building the mpi_f08 bindings, this glue needs to be built
# first because it is needed by both the mpi_f08 module itself and the
# mpi_f08-based MPI extensions.  We don't really care about the actual
# library that is built -- we just want to force the fortran modules
# to be built.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS
noinst_LTLIBRARIES += \
        libforce_usempif08_internal_modules_to_be_built.la

libforce_usempif08_internal_modules_to_be_built_la_SOURCES = \
        mpi-f08-types.F90 \
        mpi-f08-interfaces.F90 \
        mpi-f08-interfaces-callbacks.F90 \
        pmpi-f08-interfaces.F90

#
# Clean up all module files and all generated files
#

MOSTLYCLEANFILES = *.mod

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

config_h = \
    $(top_builddir)/ompi/mpi/fortran/configure-fortran-output.h \
    $(top_srcdir)/ompi/mpi/fortran/configure-fortran-output-bottom.h

mpi-f08-types.lo: $(config_h)
mpi-f08-types.lo: mpi-f08-types.F90
mpi-f08-interfaces.lo: $(config_h)
mpi-f08-interfaces.lo: mpi-f08-interfaces.F90
mpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo
mpi-f08-interfaces-callbacks.lo: $(config_h)
mpi-f08-interfaces-callbacks.lo: mpi-f08-interfaces-callbacks.F90
mpi-f08-interfaces-callbacks.lo: mpi-f08-types.lo
pmpi-f08-interfaces.lo: $(config_h)
pmpi-f08-interfaces.lo: pmpi-f08-interfaces.F90
pmpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo

# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(

install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(libdir); \
	  $(INSTALL) $$file $(DESTDIR)$(libdir); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(libdir)/$$file; \
	  rm -f $(DESTDIR)$(libdir)/$$file; \
	done
else

# Need to have empty targets because AM can't handle having an
# AM_CONDITIONAL was targets in the "if" statement but not in the
# "else".  :-(

install-exec-hook:
uninstall-local:

endif
