# -*- makefile -*-
#
# Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012	Oracle and/or its affiliates.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

if OMPI_WANT_JAVA_BINDINGS

# Get the include files that were generated from the .java source files
AM_CPPFLAGS = -I$(OSHMEM_TOP_BUILDDIR)/oshmem/shmem/java/java $(OPAL_JDK_CPPFLAGS) -DOPAL_DYN_LIB_SUFFIX=\"$(OPAL_DYN_LIB_SUFFIX)\"

headers = \
        shmemJava.h
oshmemdir = $(oshmemincludedir)/oshmem/shmem/java
oshmem_HEADERS = \
        $(headers)

lib_LTLIBRARIES = liboshmem_java.la
liboshmem_java_la_SOURCES = \
        shmem_Addr.c \
	shmem_Constant.c \
	shmem_PSync.c \
        shmem_ShMem.c

liboshmem_java_la_LIBADD = $(OSHMEM_TOP_BUILDDIR)/oshmem/liboshmem.la
liboshmem_java_la_LDFLAGS = -version-info $(liboshmem_java_so_version)

endif
