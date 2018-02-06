# Copyright © 2009-2017 Inria.  All rights reserved.
# Copyright © 2009-2010 Université Bordeaux
# Copyright © 2009-2014 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

# Add your output files here.
xml_outputs = \
        16amd64-8n2c-cpusets.xml \
        16amd64-4distances.xml \
        16amd64-4distances.console.output \
        16em64t-4s2c2t.xml \
        16em64t-4s2c2t-offlines.xml \
        16em64t-4s2c2t.console.output \
        16-2gr2gr2n2c+misc.xml \
        16-2gr2gr2n2c+misc.console.output \
        16intel64-manyVFs.xml \
        16intel64-manyVFs.console.output \
        16intel64-manyVFs.console.nocollapse.output \
        24em64t-2n6c2t-pci.xml \
        32em64t-2n8c2t-pci-noio.xml \
        32em64t-2n8c2t-pci-normalio.xml \
        32em64t-2n8c2t-pci-wholeio.xml \
        64intel64-3g2n+2n-irregulargroups+pci.xml \
        64intel64-3g2n+2n-irregulargroups+pci.console.output \
        64intel64-fakeKNL-SNC4-hybrid.xml \
        96em64t-4n4d3ca2co-pci.xml \
        192em64t-12gr2n8c2t.xml \
        192em64t-24n8c2t.xml \
        8em64t-2p2ca2co-nonodesets.v1tov2.xml \
        8ia64-2n2s2c+1n.v1tov2.xml \
        16amd64-4distances.v1tov2.xml \
        16amd64-4distances.v2tov1.xml \
        2intel64-1n2c-numaroot.v1tov2.xml \
        28intel64-2p2g7c-CoDgroups.v1tov2.xml \
        28intel64-2p2g7c-CoD.nogroups.v1tov2.xml \
        64intel64-fakeKNL-SNC4-hybrid.v1tov2.xml \
        64intel64-fakeKNL-SNC4-hybrid.v2tov1.xml

# Each output `xyz.xml' may have a corresponding `xyz.source' file specifying
# its source file.
# Each output `xyz.output' must have a corresponding `xyz.xml' source file,
# or a `xyz.source' file specifying its source file.
xml_source = \
        16amd64-4distances.console.source \
        16em64t-4s2c2t.console.source \
        16-2gr2gr2n2c+misc.console.source \
        16intel64-manyVFs.console.source \
        16intel64-manyVFs.console.nocollapse.source \
        32em64t-2n8c2t-pci-noio.source \
        32em64t-2n8c2t-pci-normalio.source \
        64intel64-3g2n+2n-irregulargroups+pci.console.source \
        8em64t-2p2ca2co-nonodesets.v1tov2.source \
        8em64t-2p2ca2co-nonodesets.v1.xml \
        8ia64-2n2s2c+1n.v1tov2.source \
        8ia64-2n2s2c+1n.v1.xml \
        16amd64-4distances.v1tov2.source \
        16amd64-4distances.v2tov1.source \
        16amd64-4distances.v1.xml \
        2intel64-1n2c-numaroot.v1tov2.source \
        2intel64-1n2c-numaroot.v1.xml \
        28intel64-2p2g7c-CoDgroups.v1tov2.source \
        28intel64-2p2g7c-CoD.nogroups.v1tov2.source \
        28intel64-2p2g7c-CoDgroups.v1.xml \
        64intel64-fakeKNL-SNC4-hybrid.v1tov2.source \
        64intel64-fakeKNL-SNC4-hybrid.v2tov1.source

# Each output `xyz.xml' or `xyz.output' may have a corresponding `xyz.options'
# file modifying the behavior of lstopo
xml_options = \
        16amd64-4distances.console.options \
        16em64t-4s2c2t.console.options \
        16intel64-manyVFs.console.options \
        16intel64-manyVFs.console.nocollapse.options \
        16-2gr2gr2n2c+misc.console.options \
        32em64t-2n8c2t-pci-noio.options \
        32em64t-2n8c2t-pci-wholeio.options \
        64intel64-3g2n+2n-irregulargroups+pci.options \
        64intel64-3g2n+2n-irregulargroups+pci.console.options \
        16amd64-4distances.v2tov1.options \
        28intel64-2p2g7c-CoD.nogroups.v1tov2.options \
        64intel64-fakeKNL-SNC4-hybrid.v2tov1.options

# Each output `xyz.xml' or `xyz.output' may have a corresponding `xyz.env'
# modifying the environment of lstopo
xml_envs = \
        16amd64-4distances.console.env \
        64intel64-3g2n+2n-irregulargroups+pci.console.env

# Only run the tests if we're building standalone, because the tests
# call hwloc executables.
TESTS = $(xml_outputs)

EXTRA_DIST = $(xml_outputs) $(xml_source) $(xml_options) $(xml_envs)

LOG_COMPILER = $(HWLOC_top_builddir)/tests/hwloc/xml/test-topology.sh
