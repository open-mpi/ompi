#
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AM_CPPFLAGS = $(common_alps_CPPFLAGS)

headers = \
	common_alps.h

sources = \
	common_alps.c


lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = lib@ORTE_LIB_PREFIX@mca_common_alps.la
comp_noinst = lib@ORTE_LIB_PREFIX@mca_common_alps_noinst.la

if MCA_BUILD_orte_common_alps_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

lib@ORTE_LIB_PREFIX@mca_common_alps_la_SOURCES = $(headers) $(sources)
lib@ORTE_LIB_PREFIX@mca_common_alps_la_CPPFLAGS = $(common_alps_CPPFLAGS)
lib@ORTE_LIB_PREFIX@mca_common_alps_la_LDFLAGS =  \
        -version-info $(libmca_orte_common_alps_so_version) \
        $(common_alps_LDFLAGS)
lib@ORTE_LIB_PREFIX@mca_common_alps_la_LIBADD = $(common_alps_LIBS)
lib@ORTE_LIB_PREFIX@mca_common_alps_noinst_la_SOURCES = $(headers) $(sources)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/opal/mca/common/common_alps.h
opal_HEADERS = $(headers)
else
opaldir = $(includedir)
endif

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically (case 2), described above).
V=0
OMPI_V_LN_SCOMP = $(ompi__v_LN_SCOMP_$V)
ompi__v_LN_SCOMP_ = $(ompi__v_LN_SCOMP_$AM_DEFAULT_VERBOSITY)
ompi__v_LN_SCOMP_0 = @echo "  LN_S    " `basename $(comp_inst)`;

all-local:
	$(OMPI_V_LN_SCOMP) if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	  $(LN_S) "$(comp_noinst)" "$(comp_inst)"; \
	fi

clean-local:
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(comp_inst)"; \
	fi
