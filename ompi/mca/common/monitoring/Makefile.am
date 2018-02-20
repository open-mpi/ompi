#
# Copyright (c) 2016      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2016      Inria.  All rights reserved.
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

EXTRA_DIST = profile2mat.pl aggregate_profile.pl

sources = common_monitoring.c common_monitoring_coll.c
headers = common_monitoring.h common_monitoring_coll.h

lib_LTLIBRARIES =
noinst_LTLIBRARIES =
component_install = libmca_common_monitoring.la
component_noinst = libmca_common_monitoring_noinst.la

if MCA_BUILD_ompi_common_monitoring_DSO
lib_LTLIBRARIES += $(component_install)
lib_LTLIBRARIES += ompi_monitoring_prof.la

ompi_monitoring_prof_la_SOURCES = monitoring_prof.c
ompi_monitoring_prof_la_LDFLAGS= \
    -module -avoid-version -shared $(WRAPPER_EXTRA_LDFLAGS)
ompi_monitoring_prof_la_LIBADD = \
    $(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la \
    $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la

if OPAL_INSTALL_BINARIES
bin_SCRIPTS = profile2mat.pl aggregate_profile.pl
endif # OPAL_INSTALL_BINARIES

else # MCA_BUILD_ompi_common_monitoring_DSO
noinst_LTLIBRARIES += $(component_noinst)
endif # MCA_BUILD_ompi_common_monitoring_DSO

libmca_common_monitoring_la_SOURCES = $(headers) $(sources)
libmca_common_monitoring_la_CPPFLAGS = $(common_monitoring_CPPFLAGS)
libmca_common_monitoring_la_LDFLAGS =  \
        -version-info $(libmca_ompi_common_monitoring_so_version) \
        $(common_monitoring_LDFLAGS)
libmca_common_monitoring_la_LIBADD = $(common_monitoring_LIBS)
libmca_common_monitoring_noinst_la_SOURCES = $(headers) $(sources)

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
	if test -z "$(lib_LTLIBRARIES)"; then \
	  rm -f "$(component_install)"; \
	fi
