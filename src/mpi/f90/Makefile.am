# -*- makefile -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = scripts

AM_FCFLAGS = -I$(top_srcdir)/include $(OMPI_FC_MODULE_FLAG) .

# Override the default f90 rules because we have to insert
# $(FCFLAGS_f90) right before the source filename.  This is necessary
# for cases where the compiler expects source files to end in .f, and
# if we want our .f90 files to be compilable, we have to insert a
# special flag right before the source filename (e.g., xlf).

.f90.o:
	$(FCCOMPILE) -c -o $@ $(FCFLAGS_f90) $<

.f90.obj:
	$(FCCOMPILE) -c -o $@ `$(CYGPATH_W) $(FCFLAGS_f90) '$<'`

.f90.lo:
	$(LTFCCOMPILE) -c -o $@ $(FCFLAGS_f90) $<

lib_LTLIBRARIES =
if OMPI_WANT_F90_BINDINGS
lib_LTLIBRARIES += libmpi_f90.la
else
lib_LTLIBRARIES +=
endif

#
# Force the mpi_kinds module to be generated before trying to compile
# any of the library files
#

$(libmpi_f90_la_SOURCES): mpi_kinds.o
$(nodist_libmpi_f90_la_SOURCES): mpi_kinds.o

#
# Print a warning indicating that compiling mpi.f90 can take a while
#

mpi.lo: long-warning
long-warning:
	@echo "***************************************************************"
	@echo "* Compiling the mpi.f90 file may take a few minutes."
	@echo "* This is quite normal -- do not be alarmed if the compile"
	@echo "* process seems to 'hang' at this point for several minutes."
	@echo "***************************************************************"

#
# Source for the f90 library
#

libmpi_f90_la_SOURCES = mpi.f90

nodist_libmpi_f90_la_SOURCES = \
     mpi-f90-interfaces.h \
     mpi_address_f90.f90 \
     mpi_bcast_f90.f90 \
     mpi_bsend_f90.f90 \
     mpi_bsend_init_f90.f90 \
     mpi_buffer_attach_f90.f90 \
     mpi_buffer_detach_f90.f90 \
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
     mpi_file_write_shared_f90.f90 \
     mpi_ibsend_f90.f90 \
     mpi_irecv_f90.f90 \
     mpi_irsend_f90.f90 \
     mpi_isend_f90.f90 \
     mpi_issend_f90.f90 \
     mpi_recv_f90.f90 \
     mpi_recv_init_f90.f90 \
     mpi_rsend_f90.f90 \
     mpi_rsend_init_f90.f90 \
     mpi_send_f90.f90 \
     mpi_send_init_f90.f90 \
     mpi_sendrecv_replace_f90.f90 \
     mpi_ssend_f90.f90 \
     mpi_ssend_init_f90.f90

if WANT_MPI2_ONE_SIDED
nodist_libmpi_f90_la_SOURCES += \
     mpi_accumulate_f90.f90 \
     mpi_get_f90.f90 \
     mpi_put_f90.f90 \
     mpi_win_create_f90.f90
endif

#        win_call_errhandler_f.c \
#        win_complete_f.c \
#        win_create_errhandler_f.c \
#        win_create_f.c \
#        win_create_keyval_f.c \
#        win_delete_attr_f.c \
#        win_fence_f.c \
#        win_free_f.c \
#        win_free_keyval_f.c \
#        win_get_attr_f.c \
#        win_get_errhandler_f.c \
#        win_get_group_f.c \
#        win_get_name_f.c \
#        win_lock_f.c \
#        win_post_f.c \
#        win_set_attr_f.c \
#        win_set_errhandler_f.c \
#        win_set_name_f.c \
#        win_start_f.c \
#        win_test_f.c \
#        win_unlock_f.c \
#        win_wait_f.c

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libmpi_f90_la_SOURCES):
	if test ! -r $@ ; then \
	    $(srcdir)/scripts/$@.sh > $@ ; \
	fi

MOSTLYCLEANFILES = *.mod

DISTCLEANFILES = $(nodist_libmpi_f90_la_SOURCES) $(BUILT_SOURCES)

#
# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(
#

if OMPI_WANT_F90_BINDINGS
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
