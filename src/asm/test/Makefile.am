#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_HEADERS = atomic_test.h

TESTS = \
	atomic_barrier \
	atomic_spinlock_serial \
	atomic_spinlock_2 \
	atomic_spinlock_5 \
	atomic_spinlock_8 \
	atomic_cmpset_32_serial \
	atomic_cmpset_64_serial \
	atomic-test

check_PROGRAMS = \
	atomic_barrier \
	atomic_spinlock_serial \
	atomic_spinlock_2 \
	atomic_spinlock_5 \
	atomic_spinlock_8 \
	atomic_cmpset_32_serial \
	atomic_cmpset_64_serial \
	atomic-test

######################################################################

atomic_barrier_SOURCES = atomic_barrier.c
atomic_barrier_LDADD = ../libasm.la

######################################################################

atomic_spinlock_serial_SOURCES = \
	atomic_spinlock_serial.c \
	atomic_spinlock.c
atomic_spinlock_serial_LDADD = ../libasm.la

atomic_spinlock_2_SOURCES = \
	atomic_spinlock_2.c \
	atomic_spinlock.c
atomic_spinlock_2_LDADD = ../libasm.la

atomic_spinlock_5_SOURCES = \
	atomic_spinlock_5.c \
	atomic_spinlock.c
atomic_spinlock_5_LDADD = ../libasm.la

atomic_spinlock_8_SOURCES = \
	atomic_spinlock_8.c \
	atomic_spinlock.c
atomic_spinlock_8_LDADD = ../libasm.la

######################################################################

atomic_cmpset_32_serial_SOURCES = atomic_cmpset_32_serial.c
atomic_cmpset_32_serial_LDADD = ../libasm.la

######################################################################

atomic_cmpset_64_serial_SOURCES = atomic_cmpset_64_serial.c
atomic_cmpset_64_serial_LDADD = ../libasm.la

######################################################################

atomic_test_SOURCES = atomic-test.c
atomic_test_LDADD = ../libasm.la
