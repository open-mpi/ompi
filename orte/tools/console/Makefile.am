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

dist_pkgdata_DATA = help-orteconsole.txt

libs = \
        $(top_builddir)/orte/liborte.la

headers = orteconsole.h

bin_PROGRAMS = \
        orteconsole

orteconsole_SOURCES = \
		$(headers) \
        orteconsole.c 

orteconsole_LDADD = $(libs) 
orteconsole_DEPENDENCIES = $(libs)

if WANT_INSTALL_HEADERS
ortedir = $(includedir)/openmpi/orte/tools/orteconsole
orte_HEADERS = $(headers)
else
ortedir = $(includedir)
endif

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)
