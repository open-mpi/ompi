#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

SUBDIRS = \
        include \
        class \
	dps \
        mca \
        runtime \
        util \
        . \
        dynamic-mca \
        tools

# Build the main ORTE library

lib_LTLIBRARIES = liborte.la
liborte_la_SOURCES =
liborte_la_LIBADD = \
        class/libclass.la \
        dps/libdps.la \
        runtime/libruntime.la \
        util/liborteutil.la \
	$(MCA_orte_FRAMEWORK_LIBS)

liborte_la_DEPENDENCIES = $(liborte_la_LIBADD)
