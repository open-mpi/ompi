#
# Copyright (c) 2018      Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Note that building this common component statically and linking
# against other dynamic components is *not* supported!

# Header files

headers = \
        common_ucx.h

# Source files

sources = \
        common_ucx.c

# Help file

# As per above, we'll either have an installable or noinst result.
# The installable one should follow the same MCA prefix naming rules
# (i.e., libmca_<type>_<name>.la).  The noinst one can be named
# whatever it wants, although libmca_<type>_<name>_noinst.la is
# recommended.

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
comp_inst = lib@OPAL_LIB_PREFIX@mca_common_ucx.la
comp_noinst = lib@OPAL_LIB_PREFIX@mca_common_ucx_noinst.la

if MCA_BUILD_opal_common_ucx_DSO
lib_LTLIBRARIES += $(comp_inst)
else
noinst_LTLIBRARIES += $(comp_noinst)
endif

lib@OPAL_LIB_PREFIX@mca_common_ucx_la_SOURCES = \
        $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_ucx_la_CFLAGS = \
		$(common_ucx_CFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_ucx_la_CPPFLAGS = \
		$(common_ucx_CPPFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_ucx_la_LDFLAGS = \
        -version-info $(libmca_opal_common_ucx_so_version) \
		$(common_ucx_LDFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_ucx_la_LIBADD = \
		$(common_ucx_LIBS) \
		$(OMPI_TOP_BUILDDIR)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
		$(OMPI_TOP_BUILDDIR)/opal/mca/memory/libmca_memory.la
lib@OPAL_LIB_PREFIX@mca_common_ucx_noinst_la_SOURCES = \
        $(headers) $(sources)
lib@OPAL_LIB_PREFIX@mca_common_ucx_noinst_la_CFLAGS = \
		$(common_ucx_CFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_ucx_noinst_la_CPPFLAGS = \
		$(common_ucx_CPPFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_ucx_noinst_la_LDFLAGS = \
		$(common_ucx_LDFLAGS)
lib@OPAL_LIB_PREFIX@mca_common_ucx_noinst_la_LIBADD = \
		$(common_ucx_LIBS)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
opaldir = $(opalincludedir)/$(subdir)
opal_HEADERS = $(headers)
endif

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically (case 2), described above).

# See Makefile.ompi-rules for an explanation of the "V" macros, below
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
