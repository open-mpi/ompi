Applied the following patches from the upstream hwloc 1.7 branch after
the v1.7.2 release:

5198d4c Only include <malloc.h> if necessary
438d9ed linux/NUMA: Work around buggy NUMA node cpusets

Applied this from the upstream hwloc master, slightly modified for this
local version of v1.7 that we have:

7489287 topology-linux.c: ensure fd is marked as close-on-exec
3aa0ed6 topology-linux.c: Stevens says we should GETFD before we SETFD, so we do
