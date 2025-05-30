# -*- makefile -*-
#
# Copyright (c) 2006-2019 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2020 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# Copyright (C) 2024      Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# Note that Automake's Fortran-buidling rules uses CPPFLAGS and
# AM_CPPFLAGS.  This can cause weirdness (e.g.,
# https://github.com/open-mpi/ompi/issues/7253).  Let's just zero
# those out and rely on AM_FCFLAGS.
CPPFLAGS =
AM_CPPFLAGS =

# This Makefile is only relevant if we're building the "use mpi_f08"
# MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

AM_FCFLAGS = -I$(top_builddir)/ompi/include \
             -I$(top_srcdir)/ompi/include \
             $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/mpi/fortran/use-mpi \
             $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/mpi/fortran/use-mpi-ignore-tkr \
             $(OMPI_FC_MODULE_FLAG). \
             -I$(top_srcdir) -I$(top_builddir) $(FCFLAGS_f90)

MOSTLYCLEANFILES = *.mod

CLEANFILES += *.i90

###########################################################################


noinst_LTLIBRARIES = libusempif08_internal_modules.la libforce_usempif08_internal_modules_to_be_built.la

# f08 support modules

libusempif08_internal_modules_la_SOURCES = \
        mpi-f08-types.F90 \
        mpi-f08-callbacks.F90 \
        mpi-f08-constants.h

libusempif08_internal_modules_la_LIBADD = \
        $(top_builddir)/ompi/mpi/fortran/use-mpi/libusempi_internal_modules.la

libforce_usempif08_internal_modules_to_be_built_la_SOURCES = \
        mpi-f08-interfaces.F90 \
        pmpi-f08-interfaces.F90 \
        mpi-f08-interfaces-callbacks.F90

nodist_noinst_HEADERS = mpi-f08-interfaces.h

noinst_HEADERS = \
	mpi-f08-rename.h \
        mpi-f08-interfaces-generated.h

libforce_usempi_internal_modules_to_be_built.la: libusempif08_internal_modules.la

config_h = \
    $(top_builddir)/ompi/mpi/fortran/configure-fortran-output.h \
    $(top_srcdir)/ompi/mpi/fortran/configure-fortran-output-bottom.h

# Generate the Fortran interfaces

include ../Makefile.prototype_files
template_files =${prototype_files:%=$(abs_top_srcdir)/ompi/mpi/fortran/use-mpi-f08/%}

if OMPI_FORTRAN_HAVE_TS
gen_ts = --generate-ts-suffix
endif


mpi-f08-interfaces-generated.h: $(template_files)
	$(OMPI_V_GEN) $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	    --builddir $(abs_top_builddir) \
	    --srcdir $(abs_top_srcdir) \
	    --output $(abs_builddir)/$@ \
	    fortran \
	    $(gen_ts) \
	    interface \
	    --prototype-files $(template_files)

# Delete generated file on maintainer-clean
MAINTAINERCLEANFILES = mpi-f08-interfaces-generated.h

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

mpi-f08-types.lo: $(config_h)
mpi-f08-types.lo: mpi-f08-types.F90
mpi-f08-interfaces.lo: $(config_h)
mpi-f08-interfaces.lo: mpi-f08-interfaces.F90
mpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo
mpi-f08-interfaces.lo: mpi-f08-interfaces.h
mpi-f08-interfaces.lo: mpi-f08-interfaces-generated.h
mpi-f08-interfaces-callbacks.lo: $(config_h)
mpi-f08-interfaces-callbacks.lo: mpi-f08-interfaces-callbacks.F90
mpi-f08-interfaces-callbacks.lo: mpi-f08-types.lo
mpi-f08-callbacks.lo: $(config_h)
mpi-f08-callbacks.lo: mpi-f08-callbacks.F90
mpi-f08-callbacks.lo: mpi-f08-types.lo
pmpi-f08-interfaces.lo: $(config_h)
pmpi-f08-interfaces.lo: pmpi-f08-interfaces.F90
pmpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo
pmpi-f08-interfaces.lo: mpi-f08-interfaces.h
pmpi-f08-interfaces.lo: mpi-f08-rename.h
pmpi-f08-interfaces.lo: mpi-f08-interfaces-generated.h

###########################################################################

# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(

install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR); \
	  $(INSTALL) $$file $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR)/$$file; \
	  rm -f $(DESTDIR)$(OMPI_FORTRAN_MODULEDIR)/$$file; \
	done

endif
