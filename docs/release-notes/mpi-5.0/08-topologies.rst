.. _release-notes-mpi-5.0-ch08:

Virtual Topologies for MPI Processes
====================================


.. admonition:: tl;dr
   :class: tip

   Chapter 8 is fully SUPPORTED in the default Open MPI build. All topology construction (Cartesian, deprecated graph, distributed graph) and every local query function live in the always-built ompi/mca/topo/base; topo_base_comm_select fill_null_pointers wires every cart/graph/dist_graph entry point from mca_topo_base\_\* regardless of which topo component is selected, so behavior is complete even with only the basic component (priority 0). Neighborhood collectives are served by default-built components: blocking by coll/basic, nonblocking and persistent _init by coll/libnbc, with correct Section 8.6 buffer ordering and MPI_PROC_NULL handling. Large-count (_c / MPI_Count) variants of the neighborhood family are present in mpi.h.in. MPI_UNWEIGHTED/MPI_WEIGHTS_EMPTY sentinels and MPI_CART/MPI_GRAPH/MPI_DIST_GRAPH constants are defined. The only nuance is rank reordering: the REQUIRED semantics (reorder=false keeps ranks; reorder=true is a permission the standard says may be a no-op) hold on a plain base build; the optional hardware-aware reorder \*quality\* for MPI_Dist_graph_create comes from the topo/treematch component, which is bundled in-tree (3rd-party/treematch) and built by default (with_treematch=yes). Disabling treematch only loses the optimization, falling back to the conformant no-reorder path -> SUPPORTED with a gate note, not CONDITIONAL. Neither in-flight PR (ABI #13280, ompio-info) touches topology.

Conformance summary
-------------------


**13** reconciled requirement(s)/behavior(s): 13 supported, 0 conditional, 0 partial, 0 unsupported, 0 missing-but-in-flight, 0 N/A (100.0% of applicable fully supported).

Supported requirements
----------------------


.. list-table::
   :header-rows: 1
   :widths: 60 40

   * - Requirement
     - Reviewers (agreement)
   * - Topology construction: MPI_Cart_create / MPI_Graph_create / MPI_Dist_graph_create / MPI_Dist_graph_create_adjacent collectively create communicators carrying cached topology
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Rank reordering: reorder=true MAY remap ranks to improve communication; reorder=false keeps identical ranks (hardware-aware remap via topo/treematch for dist_graph)
     - b1=COND/b2=SUPP/b3=COND (conflict)
   * - MPI_Dims_create computes a balanced factorization, honors pre-set positive dims[i], orders computed dims nonincreasing; negative dims / non-divisible nnodes erroneous; ndims=0 & nnodes=1 returns SUCCESS
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Local query functions: MPI_Topo_test, MPI_Cartdim_get, MPI_Cart_get, MPI_Cart_rank, MPI_Cart_coords, MPI_Cart_shift, MPI_Graphdims_get, MPI_Graph_get, MPI_Graph_neighbors[_count], MPI_Dist_graph_neighbors[_count]
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - MPI_Cart_sub collectively extracts a Cartesian subspace; MPI_Cart_map / MPI_Graph_map low-level rank-placement helpers (identity mapping allowed)
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Blocking neighborhood collectives (MPI_Neighbor_allgather[v], MPI_Neighbor_alltoall[v][w]) with Section 8.6 buffer ordering and MPI_PROC_NULL handling
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Nonblocking neighborhood collectives (MPI_Ineighbor_allgather[v], MPI_Ineighbor_alltoall[v][w]) return a request
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Persistent neighborhood collectives (MPI_Neighbor_allgather_init, allgatherv_init, alltoall_init, alltoallv_init, alltoallw_init) create persistent requests with an info argument
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - Large-count (_c / MPI_Count, MPI_Aint displ) variants of the neighborhood collective family (blocking, nonblocking, persistent _init)
     - b1=SUPP/b2=sile/b3=sile (unique)
   * - MPI_UNWEIGHTED / MPI_WEIGHTS_EMPTY sentinels: distinct non-NULL values; weighted query reflects MPI_UNWEIGHTED; zero-degree procs store no weights
     - b1=SUPP/b2=SUPP/b3=SUPP (unanimous)
   * - MPI_Topo_test returns MPI_CART / MPI_GRAPH / MPI_DIST_GRAPH / MPI_UNDEFINED; topology type constants defined
     - b1=SUPP/b2=SUPP/b3=sile (unanimous)
   * - Distributed-graph constructors accept an info argument to guide mapping; MPI_INFO_NULL always valid; no info key is mandated of an implementation
     - b1=sile/b2=SUPP/b3=sile (unique)
   * - Argument-validation error classes (e.g. MPI_Cart_rank out-of-range coords -> MPI_ERR_ARG, MPI_Cart_create ndims<0 -> MPI_ERR_TOPOLOGY) emitted under MPI_PARAM_CHECK
     - b1=sile/b2=sile/b3=SUPP (unique)
