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

check_PROGRAMS = \
	atomic_barrier \
	atomic_barrier_noinline \
	atomic_spinlock_serial \
	atomic_spinlock_serial_noinline \
	atomic_spinlock_2 \
	atomic_spinlock_2_noinline \
	atomic_spinlock_5 \
	atomic_spinlock_5_noinline \
	atomic_spinlock_8 \
	atomic_spinlock_8_noinline \
	atomic_cmpset_32_serial \
	atomic_cmpset_32_serial_noinline \
	atomic_cmpset_64_serial \
	atomic_cmpset_64_serial_noinline \
	atomic-test \
	atomic-test_noinline

TESTS = \
	$(check_PROGRAMS)

######################################################################

atomic_barrier_SOURCES = atomic_barrier.c
atomic_barrier_LDADD = $(top_builddir)/src/asm/libasm.la

atomic_barrier_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_barrier.c atomic_barrier_noinline.c
atomic_barrier_noinline_SOURCES = atomic_barrier_noinline.c
atomic_barrier_noinline_LDADD = $(top_builddir)/src/asm/libasm.la
atomic_barrier_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM

######################################################################

atomic_spinlock_serial_SOURCES = \
	atomic_spinlock_serial.c \
	atomic_spinlock.c
atomic_spinlock_serial_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_spinlock_serial_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_spinlock_serial.c atomic_spinlock_serial_noinline.c
atomic_spinlock_serial_noinline_SOURCES = \
	atomic_spinlock_serial_noinline.c \
	atomic_spinlock.c
atomic_spinlock_serial_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_spinlock_serial_noinline_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_spinlock_2_SOURCES = \
	atomic_spinlock_2.c \
	atomic_spinlock.c
atomic_spinlock_2_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_spinlock_2_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_spinlock_2.c atomic_spinlock_2_noinline.c
atomic_spinlock_2_noinline_SOURCES = \
	atomic_spinlock_2_noinline.c \
	atomic_spinlock.c
atomic_spinlock_2_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_spinlock_2_noinline_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_spinlock_5_SOURCES = \
	atomic_spinlock_5.c \
	atomic_spinlock.c
atomic_spinlock_5_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_spinlock_5_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_spinlock_5.c atomic_spinlock_5_noinline.c
atomic_spinlock_5_noinline_SOURCES = \
	atomic_spinlock_5_noinline.c \
	atomic_spinlock.c
atomic_spinlock_5_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_spinlock_5_noinline_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_spinlock_8_SOURCES = \
	atomic_spinlock_8.c \
	atomic_spinlock.c
atomic_spinlock_8_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_spinlock_8_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_spinlock_8.c atomic_spinlock_8_noinline.c
atomic_spinlock_8_noinline_SOURCES = \
	atomic_spinlock_8_noinline.c \
	atomic_spinlock.c
atomic_spinlock_8_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_spinlock_8_noinline_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)


######################################################################

atomic_cmpset_32_serial_SOURCES = atomic_cmpset_32_serial.c
atomic_cmpset_32_serial_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_cmpset_32_serial_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_cmpset_32_serial.c atomic_cmpset_32_serial_noinline.c
atomic_cmpset_32_serial_noinline_SOURCES = atomic_cmpset_32_serial_noinline.c
atomic_cmpset_32_serial_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_cmpset_32_serial_noinline_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

######################################################################

atomic_cmpset_64_serial_SOURCES = atomic_cmpset_64_serial.c
atomic_cmpset_64_serial_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic_cmpset_64_serial_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic_cmpset_64_serial.c atomic_cmpset_64_serial_noinline.c
atomic_cmpset_64_serial_noinline_SOURCES = atomic_cmpset_64_serial_noinline.c
atomic_cmpset_64_serial_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_cmpset_64_serial_noinline_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

######################################################################

atomic_test_SOURCES = atomic-test.c
atomic_test_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)

atomic-test_noinline.c:
	ln -s $(top_srcdir)/test/asm/atomic-test.c atomic-test_noinline.c
atomic_test_noinline_SOURCES = atomic-test_noinline.c
atomic_test_serial_noinline_CFLAGS = $(AM_CFLAGS) -DOMPI_DISABLE_INLINE_ASM
atomic_test_noinline_LDADD = $(top_builddir)/src/asm/libasm.la $(libs)


clean-local:
	rm -f atomic_barrier_noinline.c atomic-test_noinline.c \
	atomic_cmpset_32_serial_noinline.c \
	atomic_cmpset_64_serial_noinline.c atomic_spinlock_2_noinline.c \
	atomic_spinlock_5_noinline.c atomic_spinlock_8_noinline.c \
	atomic_spinlock_serial_noinline.c
