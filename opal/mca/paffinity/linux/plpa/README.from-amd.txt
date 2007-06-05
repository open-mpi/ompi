(from helpful AMD engineers...)

Jeff,

Bill asked me to expand on what I previously wrote by documenting the
/sys interface.  Without further ado:

All the information you need for determining the topology of a given
core is located in the /sys/devices/system/cpu/cpuX/topology/
directory, where X is a core number from 0 to N - 1, given N total
cores present on the system.  On an AMD64 system, this directory will
contain the following entries, readable in ASCII form:

          physical_package_id  - which socket am I in?
          core_id                      - in my socket, which core am
          I? (0 or 1
for dual-core CPUs)
    core_siblings               - who are my sibling cores in this
socket?  (see below)
         thread_siblings                - who are my sibling threads
          in this
socket?  (not really useful for AMD64)

The sibling fields are given as bit masks of core IDs, represented as
hexdecimal numbers delimited by commas into groups of eight for easier
readability.  So for example, given a kernel that supports a maximum
of 128 cores:

cparrott@compute-2:~> cat
/sys/devices/system/cpu/cpu0/topology/core_siblings
00000000,00000000,00000000,00000003

would tell us that cores 0 and 1 are the sibling cores of core 0.
Each core is included in its own sibling core mask, so you would still
get a meaningful result if you happened to be running on a single-core
system.

Given that these are NUMA systems, you may assume that each socket may
or may not have a region of memory associated with it.  Most systems
do, but I have seen a few that had sockets without associated memory.
My dual-Opteron Linux workstation system at my desk is a prime example
of this -- all of its memory appears to the kernel as being directly
connected to socket 0.

Unfortunately, I am not aware of an easy mechanism for determining the
memory associated with a given socket, although this information does
show up in the kernel messages buffer during boot.  (i.e. run dmesg)
Perhaps Ray might know.

------

Followup to this:

The /sys/devices/system/node/ directory tree contains the memory node
topology.  Of particular interest here is numastat entry: this will
give stats on which of this node's pages are physically located here
versus foreign nodes, for example.

Unfortunately, memory mappings for specific address ranges to nodes
are not available.  I suspect that this is probably due to the fact
that Linux uses virtual addressing everywhere in userland, so any
physical address ranges corresponding to a particular memory node are
meaningless in userland.
