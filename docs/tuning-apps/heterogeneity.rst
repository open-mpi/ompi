.. _label-heterogeneous-mpi-apps:

Building heterogeneous MPI applications
=======================================

Heterogeneous support (specifically: supporting different sized and/or
represented data types in a single MPI application run) within a
single MPI job is technically required by the MPI standard.

However, there are both theoretical and practical problems with
supporting true data heterogeneity at run-time.

Indeed, it is quite uncommon for production HPC environments to be
data-heterogeneous (e.g., natively support little-endian on some nodes
and big-endian on other nodes in the same MPI application job).

As such, supporting data heterogeneity is a feature that has fallen
into disrepair and is currently known to be broken in this release of
Open MPI.

:doc:`Contributions to fix it would be welcome! </contributing>`
