# Copyright © 2009-2017 Inria.  All rights reserved.
# Copyright © 2009-2011 Université Bordeaux
# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.

AM_CFLAGS = $(HWLOC_CFLAGS)
AM_CPPFLAGS = $(HWLOC_CPPFLAGS)
AM_LDFLAGS = $(HWLOC_LDFLAGS)

# Add your expected output file here.
# By default, it shows the output of `lstopo - -v'.
sysfs_outputs = \
		2amd64-2n.output \
		2i386-2t-hugepagesizecount.xml.output \
		2i386-2c-nohugepage.xml.output \
		2ps3-2t.output \
		4em64t-2c2t-ignore-reorder.output \
		4ia64-4s.output \
		4ppc-4c.output \
		4fake-4gr1nu1pu.output \
		256ppc-8n8s4t.output \
		256ppc-8n8s4t-nocache.output \
		256ppc-8n8s4t-nosys.output \
		28em64t-2s2n7c-buggycoresiblings.output \
		32amd64-4s2n4c-cgroup.output \
		32amd64-4s2n4c-cgroup.xml.output \
		32ppc-4n4c2c.output \
		8amd64-4n2c.output  \
		8em64t-4c2t.output \
		8em64t-2s2ca2c.output \
		8em64t-2s2ca2c-buggynuma.output \
		8em64t-2s4c-heterogeneous.output \
		8ia64-2n2s2c.output \
		8ia64-2s2c2t.output \
		8ia64-4s2c.output \
		16amd64-4n4c-cgroup-distance-merge.output \
		16amd64-8n2c.output \
		16amd64-8n2c-cpusets.output \
		16amd64-8n2c-cpusets.xml.output \
		16amd64-8n2c-cpusets_noadmin.output \
		16em64t-4s2ca2c-cpusetreorder.output \
		16em64t-4s2ca2c-cpusetreorder-merge.output \
		16em64t-4s2c2t.output \
		16em64t-4s2c2t-offlines.output \
		16em64t-4s2c2t-offlines.xml.output \
		16em64t-4s2c2t_ncaches.output \
		16em64t-4s2c2t_merge.output \
		16em64t-4s2c2t.xml.output \
		16ia64-8n2s.output \
		32em64t-2n8c+1mic.output \
		40intel64-2g2n4c+pci.output \
		40intel64-4n10c+pci-conflicts.output \
		48amd64-4d2n6c-sparse.output \
		64amd64-4s2n4ca2co.output \
		64fake-4n2s2ca2c2t.output \
		64intel64-fakeKNL-A2A-cache.output \
		64intel64-fakeKNL-SNC4-hybrid.output \
		96em64t-4n4d3ca2co.output \
		96em64t-4n4d3ca2co-forcecpuinfo.output \
		128ia64-17n4s2c.output \
		256ia64-64n2s2c.output \
		4qs22-2s2t.output \
		2s390-2c.output \
		20s390-2g6s4c.output \
		1alpha.output \
		2arm-2c.output

# Each output `xyz.output' must have a corresponding tarball `xyz.tar.bz2'
# or a corresponding `xyz.source' specifying which tarball to use
sysfs_tarballs = \
		2amd64-2n.tar.bz2 \
		2i386-2t-hugepagesizecount.tar.bz2 \
		2i386-2t-hugepagesizecount.xml.source \
		2i386-2c-nohugepage.tar.bz2 \
		2i386-2c-nohugepage.xml.source \
		2ps3-2t.tar.bz2 \
		4em64t-2c2t-ignore-reorder.tar.bz2 \
		4ia64-4s.tar.bz2 \
		4ppc-4c.tar.bz2 \
		4fake-4gr1nu1pu.tar.bz2 \
		256ppc-8n8s4t.tar.bz2 \
		256ppc-8n8s4t-nocache.source \
		256ppc-8n8s4t-nosys.source \
		28em64t-2s2n7c-buggycoresiblings.tar.bz2 \
		32amd64-4s2n4c-cgroup.tar.bz2 \
		32amd64-4s2n4c-cgroup.xml.source \
		32ppc-4n4c2c.tar.bz2 \
		8amd64-4n2c.tar.bz2  \
		8em64t-4c2t.tar.bz2 \
		8em64t-2s2ca2c.tar.bz2 \
		8em64t-2s2ca2c-buggynuma.tar.bz2 \
		8em64t-2s4c-heterogeneous.tar.bz2 \
		8ia64-2n2s2c.tar.bz2 \
		8ia64-2s2c2t.tar.bz2 \
		8ia64-4s2c.tar.bz2 \
		16amd64-4n4c-cgroup-distance-merge.tar.bz2 \
		16amd64-8n2c.tar.bz2 \
		16amd64-8n2c-cpusets.tar.bz2 \
		16amd64-8n2c-cpusets.xml.source \
		16amd64-8n2c-cpusets_noadmin.source \
		16em64t-4s2ca2c-cpusetreorder.tar.bz2 \
		16em64t-4s2ca2c-cpusetreorder-merge.source \
		16em64t-4s2c2t.tar.bz2 \
		16em64t-4s2c2t-offlines.tar.bz2 \
		16em64t-4s2c2t-offlines.xml.source \
		16em64t-4s2c2t_merge.source \
		16em64t-4s2c2t_ncaches.source \
		16em64t-4s2c2t.xml.source \
		16ia64-8n2s.tar.bz2 \
		32em64t-2n8c+1mic.tar.bz2 \
		40intel64-2g2n4c+pci.tar.bz2 \
		40intel64-4n10c+pci-conflicts.tar.bz2 \
		48amd64-4d2n6c-sparse.tar.bz2 \
		64amd64-4s2n4ca2co.tar.bz2 \
		64fake-4n2s2ca2c2t.tar.bz2 \
		64intel64-fakeKNL-A2A-cache.tar.bz2 \
		64intel64-fakeKNL-SNC4-hybrid.tar.bz2 \
		96em64t-4n4d3ca2co.tar.bz2 \
		96em64t-4n4d3ca2co-forcecpuinfo.source \
		128ia64-17n4s2c.tar.bz2 \
		256ia64-64n2s2c.tar.bz2 \
		4qs22-2s2t.tar.bz2 \
		2s390-2c.tar.bz2 \
		20s390-2g6s4c.tar.bz2 \
		1alpha.tar.bz2 \
		2arm-2c.tar.bz2

# Each output `xyz.output' may have a corresponding exclude `xyz.exclude'
sysfs_excludes = \
		256ppc-8n8s4t-nocache.exclude \
		256ppc-8n8s4t-nosys.exclude

# Each output `xyz.output' may have a corresponding `xyz.options'
# file modifying the behavior of lstopo
sysfs_options = \
		2i386-2t-hugepagesizecount.xml.options \
		2i386-2c-nohugepage.xml.options \
		4em64t-2c2t-ignore-reorder.options \
		16amd64-4n4c-cgroup-distance-merge.options \
		16amd64-8n2c-cpusets.xml.options \
		16amd64-8n2c-cpusets_noadmin.options \
		16em64t-4s2ca2c-cpusetreorder-merge.options \
		16em64t-4s2c2t-offlines.xml.options \
		16em64t-4s2c2t_ncaches.options \
		16em64t-4s2c2t_merge.options \
		16em64t-4s2c2t.xml.options \
		32amd64-4s2n4c-cgroup.xml.options \
		32em64t-2n8c+1mic.options \
		40intel64-2g2n4c+pci.options

# Each output `xyz.output' may have a corresponding `xyz.env'
# modifying the environment of lstopo
sysfs_envs = \
		2i386-2t-hugepagesizecount.xml.env \
		40intel64-2g2n4c+pci.env \
		40intel64-4n10c+pci-conflicts.env \
		96em64t-4n4d3ca2co-forcecpuinfo.env

if HWLOC_HAVE_OPENAT
if HWLOC_HAVE_BUNZIPP
TESTS = $(sysfs_outputs)
endif HWLOC_HAVE_BUNZIPP
endif HWLOC_HAVE_OPENAT

EXTRA_DIST = $(sysfs_outputs) $(sysfs_tarballs) $(sysfs_excludes) $(sysfs_options) $(sysfs_envs)

LOG_COMPILER = $(builddir)/test-topology.sh

SUBDIRS = . allowed gather
