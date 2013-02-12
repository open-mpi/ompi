#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved.
# Copyright (c) 2012      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

SUBDIRS = config contrib $(MCA_PROJECT_SUBDIRS) test
EXTRA_DIST = README INSTALL VERSION Doxyfile LICENSE autogen.pl autogen.sh \
             CMakeLists.txt README.WINDOWS.txt README.JAVA.txt

include examples/Makefile.include

dist-hook:
	csh "$(top_srcdir)/config/distscript.csh" "$(top_srcdir)" "$(distdir)" "$(OMPI_VERSION)" "$(OMPI_SVN_R)"

ACLOCAL_AMFLAGS = -I config

if OSHMEM_SUPPORT
DISTCHECK_CONFIGURE_FLAGS = --with-oshmem --disable-debug
cov:
	PATH=/hpc/local/commercial/coverity/cov-sa/bin:$$PATH && rm -rf $(PWD)/cov-build && make clean && cov-build --dir $(PWD)/cov-build make all && cov-analyze --dir $(PWD)/cov-build && cov-format-errors --dir $(PWD)/cov-build

get_tarball:
	cp $(distdir).tar.gz $(to)
endif
