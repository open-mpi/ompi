#
# Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2017      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

dist_opaldata_DATA = help-pmix-ext2x.txt

sources = \
        ext2x_local.c

nodist_headers = \
        ext2x.h

nodist_sources = \
        ext2x.c \
        ext2x_client.c \
        ext2x_component.c \
        ext2x_server_north.c \
        ext2x_server_south.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_ext2x_DSO
component_noinst =
component_install = mca_pmix_ext2x.la
else
component_noinst = libmca_pmix_ext2x.la
component_install =
endif

#
# Generate ext2x sources from the pmix2x component
#
$(nodist_headers):
	pmix2xname=`echo $@ | sed -e s/ext2x/pmix2x/g` ; \
	$(SED) 's/pmix2x/ext2x/g' $(top_srcdir)/opal/mca/pmix/pmix2x/$$pmix2xname > $@

$(sources): $(nodist_headers)

$(nodist_sources): $(nodist_headers)
	pmix2xname=`echo $@ | sed -e s/ext2x/pmix2x/g` ; \
	$(SED) 's/pmix2x/ext2x/g' $(top_srcdir)/opal/mca/pmix/pmix2x/$$pmix2xname > $@

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_ext2x_la_SOURCES = $(sources)
nodist_mca_pmix_ext2x_la_SOURCES = $(nodist_sources)
mca_pmix_ext2x_la_CFLAGS = $(opal_pmix_ext2x_CFLAGS)
mca_pmix_ext2x_la_CPPFLAGS =$(opal_pmix_ext2x_CPPFLAGS)
mca_pmix_ext2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext2x_LDFLAGS)
mca_pmix_ext2x_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(opal_pmix_ext2x_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_ext2x_la_SOURCES =$(sources)
libmca_pmix_ext2x_la_CFLAGS = $(opal_pmix_ext2x_CFLAGS)
libmca_pmix_ext2x_la_CPPFLAGS = $(opal_pmix_ext2x_CPPFLAGS)
libmca_pmix_ext2x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext2x_LDFLAGS)
libmca_pmix_ext2x_la_LIBADD = $(opal_pmix_ext2x_LIBS)

clean-local:
	$(RM) -f $(nodist_sources) $(nodist_headers)
