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

######################################################################
#
# This is a bit complicated.  If there is anything in the library,
# it will always be atomic-asm.s.  We just symlink atomic-asm.s to
# the best atomic operations available (as determined at configure
# time)
#
######################################################################
generated/@OMPI_ASM_FILE@: base/@OMPI_ASSEMBLY_ARCH@.asm
	$(PERL) "$(top_srcdir)/src/asm/generate-asm.pl" "@OMPI_ASSEMBLY_ARCH@" "@OMPI_ASSEMBLY_FORMAT@" "$(top_srcdir)/src/asm/base" "$(top_builddir)/src/asm/generated/@OMPI_ASM_FILE@"

atomic-asm.s: generated/@OMPI_ASM_FILE@
	rm -f atomic-asm.s
	@ if test -f $(top_srcdir)/src/asm/generated/@OMPI_ASM_FILE@ ; then \
	    cmd="ln -s $(top_srcdir)/src/asm/generated/@OMPI_ASM_FILE@ atomic-asm.s" ; \
	    echo "$$cmd" ; \
	    $$cmd ; \
	else \
	    cmd="ln -s $(top_builddir)/src/asm/generated/@OMPI_ASM_FILE@ atomic-asm.s" ; \
	    echo "$$cmd" ; \
	    $$cmd ; \
	fi

if OMPI_HAVE_ASM_FILE
libasm_la_SOURCES = atomic-asm.s
else
libasm_la_SOURCES =
endif

libasm_la_DEPENDENCIES = generated/@OMPI_ASM_FILE@
lib_LTLIBRARIES = libasm.la

EXTRA_DIST	= \
	asm-data.txt \
	generate-asm.pl \
	generate-all-asm.sh \
	base/AMD64.asm \
	base/IA32.asm \
	base/POWERPC32.asm \
	base/POWERPC64.asm

######################################################################

TESTS = atomic-test
check_PROGRAMS = atomic-test
atomic_test_SOURCES = atomic-test.c
atomic_test_LDADD = libasm.la

######################################################################

clean-local:
	rm -f atomic-asm.s

maintainer-clean-local:
	rm -f generated/atomic-local.s

######################################################################

#
# Copy over all the generated files
#
dist-hook:
	mkdir ${distdir}/generated
	sh generate-all-asm.sh "$(PERL)" "$(srcdir)" "$(distdir)"
