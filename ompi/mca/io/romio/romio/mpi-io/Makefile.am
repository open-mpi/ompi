#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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
# Open MPI: Note that we ignore the fortran subdirectory here; we only
# need C bindings.  We also ignore the f2c / c2f source files; we
# don't need them.
#

include $(top_srcdir)/Makefile.options

SUBDIRS = glue/openmpi

# It is unfortunately necessary to make all the filenames in the ROMIO
# distribtion confrom to the MCA prefix name rule -- there is at least
# one or two filename conflicts that potentially create problems when
# merging everything into onelibmpi.

# In developer's copies, we have the real ROMIO files, and sym links
# are made.  In distribution tarballs, the sym-linked files turn into
# real files and the original filenames do not appear.

noinst_LTLIBRARIES = libmpi-io.la
libmpi_io_la_SOURCES = \
        io_romio_close.c \
        io_romio_delete.c \
        io_romio_fsync.c \
        io_romio_get_amode.c \
        io_romio_get_atom.c \
        io_romio_get_bytoff.c \
        io_romio_get_errh.c \
        io_romio_get_extent.c \
        io_romio_get_group.c \
        io_romio_get_info.c \
        io_romio_get_posn.c \
        io_romio_get_posn_sh.c \
        io_romio_get_size.c \
        io_romio_get_view.c \
        io_romio_iotest.c \
        io_romio_iowait.c \
        io_romio_iread_at.c \
        io_romio_iread.c \
        io_romio_iread_sh.c \
        io_romio_iwrite_at.c \
        io_romio_iwrite.c \
        io_romio_iwrite_sh.c \
        mpioimpl.h \
        mpioprof.h \
        io_romio_open.c \
        io_romio_prealloc.c \
        io_romio_rd_atallb.c \
        io_romio_rd_atalle.c \
        io_romio_read_allb.c \
        io_romio_read_all.c \
        io_romio_read_alle.c \
        io_romio_read_atall.c \
        io_romio_read_at.c \
        io_romio_read.c \
        io_romio_read_ordb.c \
        io_romio_read_ord.c \
        io_romio_read_orde.c \
        io_romio_read_sh.c \
        io_romio_seek.c \
        io_romio_seek_sh.c \
        io_romio_set_atom.c \
        io_romio_set_errh.c \
        io_romio_set_info.c \
        io_romio_set_size.c \
        io_romio_set_view.c \
        io_romio_wr_atallb.c \
        io_romio_wr_atalle.c \
        io_romio_write_allb.c \
        io_romio_write_all.c \
        io_romio_write_alle.c \
        io_romio_write_atall.c \
        io_romio_write_at.c \
        io_romio_write.c \
        io_romio_write_ordb.c \
        io_romio_write_ord.c \
        io_romio_write_orde.c \
        io_romio_write_sh.c \
	io_romio_register_datarep.c \
	io_romio_mpiu_greq.c \
	mpiu_greq.h \
	io_romio_mpich2_fileutil.c

#
# Sym link the sources to conform to the MCA prefix name rule
#
dir=$(top_srcdir)/mpi-io
$(libmpi_io_la_SOURCES):
	if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '10-'` ; \
		rm -f $@; \
		ln -s $(dir)/$$pname $@ ; \
	fi
