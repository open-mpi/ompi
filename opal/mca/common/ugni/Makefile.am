# -*- indent-tabs-mode:nil -*-
#
# Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
#
# Additional copyrights may follow
#
# $HEADER$
#

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

AM_CPPFLAGS = $(common_ugni_CPPFLAGS)

component_noinst = lib@OPAL_LIB_PREFIX@mca_common_ugni_noinst.la
component_install = lib@OPAL_LIB_PREFIX@mca_common_ugni.la

if MCA_BUILD_opal_common_ugni_DSO
lib_LTLIBRARIES = $(component_install)
else
noinst_LTLIBRARIES = $(component_noinst)
endif

headers = common_ugni.h \
          common_ugni_ep.h

ugni_SOURCES = common_ugni.c \
               common_ugni_ep.c

#mcacomponentdir = $(opallibdir)
lib@OPAL_LIB_PREFIX@mca_common_ugni_la_SOURCES = $(headers) $(ugni_SOURCES)
nodist_lib@OPAL_LIB_PREFIX@mca_common_ugni_la_SOURCES = $(ugni_nodist_SOURCES)
lib@OPAL_LIB_PREFIX@mca_common_ugni_la_LIBADD = $(common_ugni_LIBS)
lib@OPAL_LIB_PREFIX@mca_common_ugni_la_LDFLAGS = \
        -version-info $(libmca_opal_common_ugni_so_version) \
        $(common_ugni_LDFLAGS)

lib@OPAL_LIB_PREFIX@mca_common_ugni_noinst_la_SOURCES = \
        $(headers) $(ugni_SOURCES)
nodist_lib@OPAL_LIB_PREFIX@mca_common_ugni_noinst_la_SOURCES = \
        $(ugni_nodist_SOURCES)
lib@OPAL_LIB_PREFIX@mca_common_ugni_noinst_la_LIBADD = $(common_ugni_LIBS)
lib@OPAL_LIB_PREFIX@mca_common_ugni_noinst_la_LDFLAGS = \
        -module -avoid-version $(common_ugni_LDFLAGS)

# These two rules will sym link the "noinst" libtool library filename
# to the installable libtool library filename in the case where we are
# compiling this component statically (case 2), described above).

V=0
OMPI_V_LN_SCOMP = $(ompi__v_LN_SCOMP_$V)
ompi__v_LN_SCOMP_ = $(ompi__v_LN_SCOMP_$AM_DEFAULT_VERBOSITY)
ompi__v_LN_SCOMP_0 = @echo "  LN_S    " `basename $(component_install)`;

all-local:
	$(OMPI_V_LN_SCOMP) if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(component_install)"; \
	  $(LN_S) "$(component_noinst)" "$(component_install)"; \
	fi

clean-local:
	if test -z "$(mcacomponent_LTLIBRARIES)"; then \
	  rm -f "$(component_install)"; \
	fi
