# -*- makefile -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = scripts

# THIS MAKEFILE IS OLD AND ONLY TO SUPPORT FORTRAN COMPILERS THAT DO
# NOT SUPPORT "IGNORE TKR" FUNCTIONALITY (cough cough gfortran cough
# cough).  All reasonable Fortran compilers support ignore TKR and
# don't compile this directory -- instead, they compile
# ompi/fortran/use-mpi-ignore-tkr.  

# This tree is left over to support non-ignore-tkr compilers, but
# hasn't been updated much.  Indeed, many of the comments pre-date
# ignore-tkr days and haven't been updated.

if OMPI_BUILD_FORTRAN_USEMPI_TKR_BINDINGS

########################################################################

# This Makefile.am is quite complex and confusing.  Part of the
# problem is that Libtool (v1.5.18) does not understand F90, so we
# have to do a few things manually (and no shared libraries). Here's a
# summary of what is happening:
#
# Depedencies / who generates whom:
#
# configure generates fortran_kinds.sh
#       |
#      \|/
# fortran_kinds.sh ----> sourced by scripts/* ----> generates mpi_*.f90
#                                 |                         |
#                                \|/                        |
#                    generates mpi-f90-interfaces.h         |
#                    /                                      |
#                   \/                                      |
#  mpi-f90-interfaces.h                                     |
#                 |                                         |
#                \|/                                       \|/
#            used by mpi.F90                 used by mpi_*.f90
#           /        |                               |
#          /        \|/                             \|/
#         /  generates mpi.o                 generates mpi_*.o
#        \/                 \               /
# generates mpi module      \/             \/
# (filename unknown)      generates libmpi_f90.a
#
# Once you understand this diagram, this Makefile is much more clear.
# 

AM_FCFLAGS = -I$(top_builddir)/ompi/include -I$(top_srcdir)/ompi/include \
             $(OMPI_FC_MODULE_FLAG). -I$(srcdir) \
             -I$(top_builddir)/ompi/mpi/fortran/use-mpi-tkr $(FCFLAGS_f90)

# Do different things if the top-level configure decided that we're
# going to build F90 bindings or not.

lib_LTLIBRARIES =
if OMPI_BUILD_FORTRAN_USEMPI_TKR_BINDINGS

# Add the f90 library to the list of libraries to build

lib_LTLIBRARIES += libmpi_usempi.la

# Ensure that the F90 interfaces are re-generated based on the values
# that come in from configure

mpi-f90-interfaces.h: fortran_kinds.sh

CLEANFILES += mpi-f90-interfaces.h

# Run scripts to generate the f90 source files.  We only need to
# generate the .f90 files once, so the "test" checks to see if the
# file exists before running the script.  However, the
# mpi-f90-interfaces.h file may need to be generated multiple times
# (even if it already exists), such as if someone re-runs configure
# and changes the value fortran_kinds.sh (which is taken care of by
# the mpi-f90-interfaces.h dependency on fortran_kinds.sh, above).

mpi-f90-interfaces.h: $(srcdir)/scripts/mpi-f90-interfaces.h.sh
	@ p="`pwd`"; \
	echo $(srcdir)/scripts/mpi-f90-interfaces.h.sh $$p \> $@; \
	$(srcdir)/scripts/mpi-f90-interfaces.h.sh $$p > $@

# Unbelievably, some versions of sh (cough cough Solaris 9 cough
# cough) actually seem to internally perform a "cd" into a
# subdirectory when you run "./foo/bar", such that if you try to
# source a script in the top-level directory in the bar script (e.g.,
# ". some_script" in the bar script), it will try to run it in the
# "foo" subdirectory, rather than the top-level directory!  #$@#$%#$%
# So we have to pass in the pwd to the scripts so that they know where
# some_script is.

$(nodist_libmpi_usempi_la_SOURCES): fortran_kinds.sh
	@ p="`pwd`"; \
	echo $(srcdir)/scripts/$@.sh $$p \> $@; \
	$(srcdir)/scripts/$@.sh $$p > $@ ;

# Automake doesn't know how to do F90 dependency analysis, so manually
# list this here (i.e., "mpi-f90-interfaces.h" is included in
# mpi.F90).

# Print a warning indicating that compiling mpi.F90 can take a while.
# The only way to do this portably is to have our own rules for mpi.o
# and mpi.obj (having a "fake" target that just has echo statement in
# it may trigger mpi.o|obj to be rebuilt because the fake name doesn't
# exist; there aren't good portable ways to indicate that the target
# name is fake and should never exist).

# Note the "-I." in the commands below.  This is because mpi.F90
# includes mpi-f90-interfaces.h, which will be in the build tree
# (i.e., the current directory) because it is generated.

mpi.o: mpi.F90 mpi-f90-interfaces.h
	@echo "***************************************************************"
	@echo "* Compiling the mpi.F90 file may take a few minutes."
	@echo "* This is quite normal -- do not be alarmed if the compile"
	@echo "* process seems to 'hang' at this point for several minutes."
	@echo "***************************************************************"
	$(FCCOMPILE) -c -I. -o $@ $(FCFLAGS_f90) $<

mpi.obj: mpi.F90 mpi-f90-interfaces.h
	@echo "***************************************************************"
	@echo "* Compiling the mpi.F90 file may take a few minutes."
	@echo "* This is quite normal -- do not be alarmed if the compile"
	@echo "* process seems to 'hang' at this point for several minutes."
	@echo "***************************************************************"
	$(FCCOMPILE) -c -I. -o $@ $(FCFLAGS_f90) `$(CYGPATH_W) '$<'`

BUILT_SOURCES = mpi-f90-interfaces.h
endif

# mpi.F90 is the only f90 source file directly in libmpif90 that gets
# distributed.  

libmpi_usempi_la_SOURCES = mpi.F90
libmpi_usempi_la_LIBADD = \
     $(top_builddir)/ompi/mpi/fortran/mpif-h/libmpi_mpifh.la

# These files are all generated by scripts in the scripts/ directory.

nodist_libmpi_usempi_la_SOURCES = \
     mpi_sizeof.f90 \
     mpi_comm_spawn_multiple_f90.f90 \
     mpi_testall_f90.f90 \
     mpi_testsome_f90.f90 \
     mpi_waitall_f90.f90 \
     mpi_waitsome_f90.f90 \
     mpi_wtick_f90.f90 \
     mpi_wtime_f90.f90 \
     mpi_address_f90.f90 \
     mpi_accumulate_f90.f90 \
     mpi_bcast_f90.f90 \
     mpi_bsend_f90.f90 \
     mpi_bsend_init_f90.f90 \
     mpi_buffer_attach_f90.f90 \
     mpi_buffer_detach_f90.f90 \
     mpi_get_f90.f90 \
     mpi_get_address_f90.f90 \
     mpi_ibcast_f90.f90 \
     mpi_ibsend_f90.f90 \
     mpi_imrecv_f90.f90 \
     mpi_irecv_f90.f90 \
     mpi_irsend_f90.f90 \
     mpi_isend_f90.f90 \
     mpi_issend_f90.f90 \
     mpi_free_mem_f90.f90 \
     mpi_mrecv_f90.f90 \
     mpi_put_f90.f90 \
     mpi_recv_f90.f90 \
     mpi_recv_init_f90.f90 \
     mpi_rsend_f90.f90 \
     mpi_rsend_init_f90.f90 \
     mpi_send_f90.f90 \
     mpi_send_init_f90.f90 \
     mpi_ssend_f90.f90 \
     mpi_ssend_init_f90.f90 \
     mpi_win_create_f90.f90

if OMPI_PROVIDE_MPI_FILE_INTERFACE
nodist_libmpi_usempi_la_SOURCES += \
     mpi_file_iread_f90.f90 \
     mpi_file_iread_at_f90.f90 \
     mpi_file_iread_shared_f90.f90 \
     mpi_file_iwrite_f90.f90 \
     mpi_file_iwrite_at_f90.f90 \
     mpi_file_iwrite_shared_f90.f90 \
     mpi_file_read_f90.f90 \
     mpi_file_read_all_f90.f90 \
     mpi_file_read_all_begin_f90.f90 \
     mpi_file_read_all_end_f90.f90 \
     mpi_file_read_at_f90.f90 \
     mpi_file_read_at_all_f90.f90 \
     mpi_file_read_at_all_begin_f90.f90 \
     mpi_file_read_at_all_end_f90.f90 \
     mpi_file_read_ordered_f90.f90 \
     mpi_file_read_ordered_begin_f90.f90 \
     mpi_file_read_ordered_end_f90.f90 \
     mpi_file_read_shared_f90.f90 \
     mpi_file_write_f90.f90 \
     mpi_file_write_all_f90.f90 \
     mpi_file_write_all_begin_f90.f90 \
     mpi_file_write_all_end_f90.f90 \
     mpi_file_write_at_f90.f90 \
     mpi_file_write_at_all_f90.f90 \
     mpi_file_write_at_all_begin_f90.f90 \
     mpi_file_write_at_all_end_f90.f90 \
     mpi_file_write_ordered_f90.f90 \
     mpi_file_write_ordered_begin_f90.f90 \
     mpi_file_write_ordered_end_f90.f90 \
     mpi_file_write_shared_f90.f90
endif

# Set the library version
libmpi_usempi_la_LDFLAGS = \
        -version-info $(libmpi_usempi_tkr_so_version) \
        $(OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS)

#
# Clean up all F90 module files and all generated files
#

MOSTLYCLEANFILES = *.mod
DISTCLEANFILES = $(nodist_libmpi_usempi_la_SOURCES)

#
# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(
#

if OMPI_BUILD_FORTRAN_USEMPI_TKR_BINDINGS
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

# if OMPI_BUILD_FORTRAN_USEMPI_TKR_BINDINGS
endif
