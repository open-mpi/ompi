#
# Copyright (c) 2014-2018 Intel, Inc.  All rights reserved.
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

dist_opaldata_DATA = help-pmix-ext4x.txt

sources = \
        ext4x_local.c

nodist_headers = \
        ext4x.h

nodist_sources = \
        ext4x.c \
        ext4x_client.c \
        ext4x_component.c \
        ext4x_server_north.c \
        ext4x_server_south.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_pmix_ext4x_DSO
component_noinst =
component_install = mca_pmix_ext4x.la
else
component_noinst = libmca_pmix_ext4x.la
component_install =
endif

#
# Generate ext4x sources from the pmix4x component
#
$(nodist_headers):
	pmix4xname=`echo $@ | sed -e s/ext4x/pmix4x/g` ; \
	$(SED) 's/pmix4x/ext4x/g' $(top_srcdir)/opal/mca/pmix/pmix4x/$$pmix4xname > $@

$(sources): $(nodist_headers)

$(nodist_sources): $(nodist_headers)
	pmix4xname=`echo $@ | sed -e s/ext4x/pmix4x/g` ; \
	$(SED) 's/pmix4x/ext4x/g' $(top_srcdir)/opal/mca/pmix/pmix4x/$$pmix4xname > $@

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_pmix_ext4x_la_SOURCES = $(sources)
nodist_mca_pmix_ext4x_la_SOURCES = $(nodist_sources)
mca_pmix_ext4x_la_CFLAGS = $(opal_pmix_ext4x_CFLAGS)
mca_pmix_ext4x_la_CPPFLAGS =$(opal_pmix_ext4x_CPPFLAGS)
mca_pmix_ext4x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext4x_LDFLAGS)
mca_pmix_ext4x_la_LIBADD = $(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la \
	$(opal_pmix_ext4x_LIBS)

noinst_LTLIBRARIES = $(component_noinst)
libmca_pmix_ext4x_la_SOURCES =$(sources)
nodist_libmca_pmix_ext4x_la_SOURCES = $(nodist_sources)
libmca_pmix_ext4x_la_CFLAGS = $(opal_pmix_ext4x_CFLAGS)
libmca_pmix_ext4x_la_CPPFLAGS = $(opal_pmix_ext4x_CPPFLAGS)
libmca_pmix_ext4x_la_LDFLAGS = -module -avoid-version $(opal_pmix_ext4x_LDFLAGS)
libmca_pmix_ext4x_la_LIBADD = $(opal_pmix_ext4x_LIBS)

clean-local:
	$(RM) -f $(nodist_sources) $(nodist_headers)
