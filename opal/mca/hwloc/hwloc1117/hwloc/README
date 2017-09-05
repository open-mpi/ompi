Introduction

hwloc provides command line tools and a C API to obtain the hierarchical map of
key computing elements, such as: NUMA memory nodes, shared caches, processor
packages, processor cores, processing units (logical processors or "threads")
and even I/O devices. hwloc also gathers various attributes such as cache and
memory information, and is portable across a variety of different operating
systems and platforms. Additionally it may assemble the topologies of multiple
machines into a single one so as to let applications consult the topology of an
entire fabric or cluster at once.

hwloc primarily aims at helping high-performance computing (HPC) applications,
but is also applicable to any project seeking to exploit code and/or data
locality on modern computing platforms.

hwloc supports the following operating systems:

  * Linux (including old kernels not having sysfs topology information, with
 knowledge of cpusets, ScaleMP vSMP and Kerrighed support, etc.) on all
 supported hardware, including Intel Xeon Phi (KNL and KNC, either
 standalone or as a coprocessor) and NumaScale NumaConnect.
  * Solaris
  * AIX
  * Darwin / OS X
  * FreeBSD and its variants (such as kFreeBSD/GNU)
  * NetBSD
  * OSF/1 (a.k.a., Tru64)
  * HP-UX
  * Microsoft Windows
  * IBM BlueGene/Q Compute Node Kernel (CNK)

Since it uses standard Operating System information, hwloc's support is mostly
independant from the processor type (x86, powerpc, ...) and just relies on the
Operating System support. The only exception to this is kFreeBSD, which does
not support topology information, and hwloc thus uses an x86-only CPUID-based
backend (which can be used for other OSes too, see the Components and plugins
section).

To check whether hwloc works on a particular machine, just try to build it and
run lstopo or lstopo-no-graphics. If some things do not look right (e.g. bogus
or missing cache information), see Questions and Bugs below.

hwloc only reports the number of processors on unsupported operating systems;
no topology information is available.

For development and debugging purposes, hwloc also offers the ability to work
on "fake" topologies:

  * Symmetrical tree of resources generated from a list of level arities, see
 Synthetic topologies.
  * Remote machine simulation through the gathering of topology as XML files,
 see Importing and exporting topologies from/to XML files.

hwloc can display the topology in a human-readable format, either in graphical
mode (X11), or by exporting in one of several different formats, including:
plain text, PDF, PNG, and FIG (see Command-line Examples below). Note that some
of the export formats require additional support libraries.

hwloc offers a programming interface for manipulating topologies and objects.
It also brings a powerful CPU bitmap API that is used to describe topology
objects location on physical/logical processors. See the Programming Interface
below. It may also be used to binding applications onto certain cores or memory
nodes. Several utility programs are also provided to ease command-line
manipulation of topology objects, binding of processes, and so on.

Perl bindings are available from Bernd Kallies on CPAN.

Python bindings are available from Guy Streeter:

  * Fedora RPM and tarball.
  * git tree (html).



See https://www.open-mpi.org/projects/hwloc/doc/ for more hwloc documentation.
