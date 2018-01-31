#
# Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2018 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_opaldata_DATA = help-pmix-ext3x.txt

sources = \
        ext3x_local.c

nodist_headers = \
        ext3x.h

nodist_sources = \
        ext3x.c \
        ext3x_client.c \
        ext3x_component.c \
        ext3x_server_north.c \
        ext3x_server_south.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_ext3x_DSO
component_noinst =
component_install = mca_pmix_ext3x.la
else
component_noinst = libmca_pmix_ext3x.la
component_install =
endif

#
# Generate ext3x sources from the pmix3x component
#
$(nodist_headers):
	pmix3xname=`echo $@ | sed -e s/ext3x/pmix3x/g` ; \
	$(SED) 's/pmix3x/ext3x/g' $(top_srcdir)/opal/mca/pmix/pmix3x/$$pmix3xname > $@

$(sources): $(nodist_headers)

$(nodist_sources): $(nodist_headers)
	pmix3xname=`echo $@ | sed -e s/ext3x/pmix3x/g` ; \
	$(SED) 's/pmix3x/ext3x/g' $(top_srcdir)/opal/mca/pmix/pmix3x/$$pmix3xname > $@

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_ext3x_la_SOURCES = $(sources)
nodist_mca_pmix_ext3x_la_SOURCES = $(nodist_sources)
mca_pmix_ext3x_la_CFLAGS = $(opal_pmix_ext3x_CFLAGS)
mca_pmix_ext3x_la_CPPFLAGS =$(opal_pmix_ext3x_CPPFLAGS)
mca_pmix_ext3x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext3x_LDFLAGS)
mca_pmix_ext3x_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(opal_pmix_ext3x_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_ext3x_la_SOURCES =$(sources)
nodist_libmca_pmix_ext3x_la_SOURCES = $(nodist_sources)
libmca_pmix_ext3x_la_CFLAGS = $(opal_pmix_ext3x_CFLAGS)
libmca_pmix_ext3x_la_CPPFLAGS = $(opal_pmix_ext3x_CPPFLAGS)
libmca_pmix_ext3x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext3x_LDFLAGS)
libmca_pmix_ext3x_la_LIBADD = $(opal_pmix_ext3x_LIBS)

clean-local:
	$(RM) -f $(nodist_sources) $(nodist_headers)
