#
# $HEADER$
#
# Open MPI: Note that we ignore the fortran subdirectory here; we only
# need C bindings.  We also ignore the f2c / c2f source files; we
# don't need them.
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -DOMPI_BUILDING=1 \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmpi-io.la

libmpi_io_la_SOURCES = \
        close.c \
        delete.c \
        fsync.c \
        get_amode.c \
        get_atom.c \
        get_bytoff.c \
        get_errh.c \
        get_extent.c \
        get_group.c \
        get_info.c \
        get_posn.c \
        get_posn_sh.c \
        get_size.c \
        get_view.c \
        ioreq_c2f.c \
        ioreq_f2c.c \
        iotest.c \
        iowait.c \
        iread_at.c \
        iread.c \
        iread_sh.c \
        iwrite_at.c \
        iwrite.c \
        iwrite_sh.c \
        mpioimpl.h \
        mpioprof.h \
        open.c \
        prealloc.c \
        rd_atallb.c \
        rd_atalle.c \
        read_allb.c \
        read_all.c \
        read_alle.c \
        read_atall.c \
        read_at.c \
        read.c \
        read_ordb.c \
        read_ord.c \
        read_orde.c \
        read_sh.c \
        seek.c \
        seek_sh.c \
        set_atom.c \
        set_errh.c \
        set_info.c \
        set_size.c \
        set_view.c \
        wr_atallb.c \
        wr_atalle.c \
        write_allb.c \
        write_all.c \
        write_alle.c \
        write_atall.c \
        write_at.c \
        write.c \
        write_ordb.c \
        write_ord.c \
        write_orde.c \
        write_sh.c
