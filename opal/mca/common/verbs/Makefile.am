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

AM_CPPFLAGS = $(common_verbs_CPPFLAGS)

headers = \
	common_verbs.h

sources = \
	common_verbs_basics.c \
	common_verbs_devlist.c \
	common_verbs_find_max_inline.c \
	common_verbs_find_ports.c \
	common_verbs_mca.c \
	common_verbs_port.c \
	common_verbs_qp_type.c

dist_opaldata_DATA = \
        help-opal-common-verbs.txt

# To simplify components that link to this library, we will *always*
# have an output libtool library named libmca_<type>_<name>.la -- even
# for case 2) described above (i.e., so there's no conditional logic
# necessary in component Makefile.am's that link to this library).
# Hence, if we're creating a noinst version of this library (i.e.,
# case 2), we sym link it to the libmca_<type>_<name>.la name
# (libtool will do the Right Things under the covers).  See the
# all-local and clean-local rules, below, for how this is effected.

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
comp_inst = lib@OPAL_LIB_PREFIX@mca_common_verbs.la
comp_noinst = lib@OPAL_LIB_PREFIX@mca_common_verbs_noinst.la

if MCA_BUILD_opal_common_verbs_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

lib@OPAL_LIB_PREFIX@mca_common_verbs_la_SOURCES = $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_verbs_la_CPPFLAGS = $(common_verbs_CPPFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_verbs_la_LDFLAGS =  \
        -version-info $(libmca_opal_common_verbs_so_version) \
        $(common_verbs_LDFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_verbs_la_LIBADD = $(common_verbs_LIBS)
lib@OPAL_LIB_PREFIX@mca_common_verbs_noinst_la_SOURCES = $(headers) $(sources)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/opal/mca/common/verbs
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
