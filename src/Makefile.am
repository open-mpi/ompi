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
        file \
        group \
        info \
        class \
        mca \
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
        dynamic-mca \
        tools

lib_LTLIBRARIES = libmpi.la
libmpi_la_SOURCES =
libmpi_la_LIBADD = \
        $(LIBLTDL_LTLIB) \
        attribute/libattribute.la \
        communicator/libcommunicator.la \
        ctnetwork/libctnetwork.la \
        datatype/libdatatype.la \
        errhandler/liberrhandler.la \
        event/libevent.la \
        group/libgroup.la \
        info/libinfo.la \
        class/liblfc.la \
        mca/libmca.la \
        mpi/libmpi_bindings.la \
        op/libop.la \
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
