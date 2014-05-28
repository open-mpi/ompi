#
# Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012      Sandia National Laboratories. All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This library is shared among all the consumers of PMI to provide a
# single point for initializing and shutting down PMI.
#
# This is not a component in the traditional sense.  It is a library
# (either rolled into libmpi if the components are rolled into libmpi
# or a stand-alone library).  Users of this "component" must
# explicitly link against libmca_common_pmi.la when building
# stand-alone libraries (it won't hurt to do so in all cases).
#
# Note that building this common component statically and linking
# against other dynamic components is *not* supported!

AM_CPPFLAGS = $(common_pmi_CPPFLAGS)

dist_ompidata_DATA = help-common-pmi.txt

# control whether building an installed library or a convenience
# (noinst) library
if MCA_BUILD_opal_common_pmi_DSO
component_noinst =
component_install = libmca_common_pmi.la
else
component_noinst = libmca_common_pmi.la
component_install =
endif

lib_LTLIBRARIES = $(component_install)
noinst_LTLIBRARIES = $(component_noinst)
libmca_common_pmi_la_SOURCES = common_pmi.h common_pmi.c
libmca_common_pmi_la_LDFLAGS = $(common_pmi_LDFLAGS) -version-info $(libmca_opal_common_pmi_so_version)
libmca_common_pmi_la_LIBADD = $(common_pmi_LIBS)
