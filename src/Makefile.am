# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = \
        include \
        $(LIBLTDL_SUBDIR) \
        attribute \
        communicator \
        ctnetwork \
        datatype \
        errhandler \
        event \
        group \
        info \
        lfc \
        mca \
        mem \
        mpi \
        op \
        os \
        proc \
        request \
        runtime \
        threads \
        util \
        win \
        . \
        tools

not_yet = \
        file

lib_LTLIBRARIES = libmpi.la
libmpi_la_SOURCES =
libmpi_la_LIBADD = \
        $(LIBLTDL_LTLIB) \
        attribute/libattribute.la \
        communicator/libcommunicator.la \
        ctnetwork/libctnetwork.la \
        datatype/libdatatype.la \
        event/libevent.la \
        group/libgroup.la \
        info/libinfo.la \
        lfc/liblfc.la \
        mca/libmca.la \
        mem/libmem.la \
        mpi/libmpi_bindings.la \
        proc/libproc.la \
        request/librequest.la \
        runtime/libruntime.la \
        threads/libthreads.la \
        util/libutil.la \
        win/libwin.la

not_implemented_yet =\
        errhandler/liberrhandler.la \
        file/libfile.la \
        op/libop.la \
        os/libos.la
