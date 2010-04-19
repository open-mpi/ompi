# Copyright 2009 INRIA, Université Bordeaux 1
# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

# Add your XML input files here.
xml_inputs = \
	16amd64-8n2c-cpusets.xml \
	16em64t-4s2c2t.xml \
	16em64t-4s2c2t-offlines.xml \
	8em64t-2mi2ma2c.xml

# Only run the tests if we're building standalone, because the tests
# call hwloc executables.
if HWLOC_BUILD_TESTS
if HWLOC_HAVE_XML
TESTS = $(xml_inputs)
endif HWLOC_HAVE_XML
endif HWLOC_BUILD_TESTS

EXTRA_DIST = $(xml_inputs)

TESTS_ENVIRONMENT = $(HWLOC_top_builddir)/tests/xml/test-topology.sh
