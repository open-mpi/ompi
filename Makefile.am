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

SUBDIRS = config include etc src
EXTRA_DIST = README INSTALL VERSION Doxyfile LICENSE

dist-hook:
	csh config/distscript.csh $(PACKAGE)-$(OMPI_VERSION) $(OMPI_VERSION)
