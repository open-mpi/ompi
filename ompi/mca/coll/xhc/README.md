# XHC: XPMEM-based Hierarchical Collectives

XHC implements hierarchical & topology-aware intra-node MPI collectives,
utilizing XPMEM for efficient shared address space memory access between
processes.

## Main features

* XHC constructs an **n-level hierarchy** (i.e. no algorithmic limitation on
level count), based on intra-node topological features. Rank/process locality
information is known thanks to Hwloc, and is obtained from Open MPI's
integrated book-keeping.
	
	Topological features that can currently be defined:
	
	- NUMA node
	- CPU Socket
	- L1/L2/L3 cache
	- Hwthread/core
	- Node (all ranks *are* in same node --> flat, no hierarchy at all)
	
	Example of a 3-level XHC hierarchy (numa+socket+node configuration):
	
	![Example of 3-level XHC hierarchy](resources/xhc-hierarchy.svg)
	
	Furthermore, support for custom virtual user-defined hierarchies is
	available, to allow fine-grained control over the communication pattern.

* **Single-copy** transportation
	
	- Supported through integration with Open MPI's `opal/smsc`
	(shared-memory-single-copy) framework. Selecting `smsc/xpmem` is highly
	recommended.
	
		- Bcast support: XPMEM, CMA, KNEM
		- Allreduce/Reduce support: XPMEM
		- Barrier support: *(irrelevant)*
	
	- Application buffers are attached on the fly the first time they appear,
	saved on and recovered from the registration cache in subsequent
	appearances. (assuming smsc/xpmem)

* **Copy-in-copy-out (CICO)** transportation
	
	- Through shared memory buffers that remain active throughout the
	component's lifetime.
	
	- Switchover with single-copy at configurable message size.
	
	- Supported in all ops, regardless of smsc support or XPMEM presence (up to
	maximum allowed message size).

* **Inline** transportation
	
	- For especially small messages, payload data is inlined in the same cache
	line as the control data.
	
	- Supported in all ops, regardless of smsc support or XPMEM presence (up to
	maximum allowed message size).

* Data-wise **pipelining** across all levels of the hierarchy. Allows for
lowering hierarchy-induced start-up overheads, and interleaving of operations
in applicable operations (e.g. reduce+bcast in allreduce).

* **Lock-free** single-writer synchronization, with appropriate cache-line
separation where necessary. Consistency ensured via lightweight *read* or
*write* memory barriers.

## Configuration options -- MCA params

XHC can be customized via a number of standard Open MPI MCA parameters, though
defaults that should satisfy a wide number of systems are in place.

The available parameters (also found in `coll_xhc_component.c`):

#### *(prepend with "coll_xhc_")*

* **priority** (default `0`): The priority of the coll/xhc component, used
during the component selection process.

* **print_info** (default `false`): Print information about XHC's generated
hierarchy and its configuration.

* **shmem_backing** (default `/dev/shm`): Backing directory for shmem files
used for XHC's synchronization fields and CICO buffers.

* **dynamic_leader** (default `false`): Enables the feature that dynamically
elects an XHC-communicator leader at each collective (currently only applicable
for bcast).

* **dynamic_reduce** (default `1`=`non-float`): Enables support for
out-of-order reduction. Ranks fetch data to reduce from multiple peers;
out-of-order reduction allows them to temporarily skip a peer when the expected
data is not yet prepared, instead of stalling. The default value auto-enables
it when the data is of non-float type; setting to `2`=`enabled for all types`,
might/will harm reproducibility of reductions with float types.

* **reduce_load_balance** (default `0`=`non-leader`): Controls the
leader-to-member load balancing mode in reductions. Under `non-leader`, the
members, and not the leaders, perform reductions. With `top-level`, all members
as well as the leader of the top-most level perform reductions. With
`first-chunk`, leaders perform a single reduction on each level for a single
chunk at the beginning of the operation. `top+first` combines `top-level` and
`first-chunk`. Finally, with `all`, all ranks perform reductions equally.

* **hierarchy** (default `"numa,socket"`): A comma separated list of
topological feature to which XHC's hierarchy-building algorithm should be
sensitive. `ompi_info` reports the possible values for the parameter.
	
	- In some ways, this is "just" a suggestion. The resulting hierarchy may
	not exactly match the requested one. Reasons that this will occur:
		
		- A requested topological feature does not effectively segment the set
		of ranks. (eg. `numa` was specified, but all ranks reside in the same
		NUMA node)
		
		- No feature that all ranks have in common was provided. This a more
		intrinsic detail, that you probably don't need to be aware of, but you
		might come across if eg. you investigate the output of `print_info`. An
		additional level will automatically be added in this case, no need to
		worry about it.
			
			For all intents and purposes, a hierarchy of `numa,socket` is
			interpreted as "segment the ranks according to NUMA node locality,
			and then further segment them according to CPU socket locality".
		
		- The provided features will automatically be re-ordered when their
		order does not match their order in the physical system. (unless a
		virtual hierarchy is present in the list)
	
	- *Virtual Hierarchies*: The string may alternatively also contain "rank
	lists" which specify exactly which ranks to group together, as well as some
	other special modifiers. See
	`coll_xhc_component.c:xhc_component_parse_hierarchy()` for further
	explanation as well as syntax information.

* **chunk_size** (default `16K`): The chunk size for the pipelining process.
Data is processed (eg broadcast, reduced) in this-much sized pieces at once.
	
	- It's possible to have a different chunk size for each level of the
	hierarchy, achieved via providing a comma-separated list of sizes (eg.
	`"16K,16K,128K"`) instead of single one. The sizes in this list's *DO NOT*
	correspond to the items on hierarchy list; the hierarchy keys might be
	re-ordered or reduced to match the system, but the chunk sizes will be
	consumed in the order they are given, left-to-right -> bottom-to-top.

* **uniform_chunks** (default `true`): Automatically optimize the chunk size
in reduction collectives, according to the message size, so that all members
will perform equal work.

* **uniform_chunks_min** (default `1K`): The lowest allowed value for the chunk
size when uniform chunks are enabled.

* **cico_max** (default `1K`): Copy-in-copy-out, instead of single-copy, will
be used for messages of *cico_max* or less bytes.

*(Removed Parameters)*

* **rcache_max**, **rcache_max_global** *(REMOVED with shift to opal/smsc)*:
Limit to number of attachments that the registration cache should hold.
	
	- A case can be made about their usefulness. If desired, shall be
	re-implemented at smsc-level.

## Limitations

- *Intra-node support only*
	- Define XHC as `coll/HAN`'s intra-node component to reap its benefits in
	multi-node runs.

- **Heterogeneity**: XHC does not support nodes with non-uniform (rank-wise)
datatype representations. (determined according to Open MPI's `proc_arch`)

- **Non-commutative** operators are not supported by XHC's reduction
collectives. In past versions, they were, but only with a flat hierarchy; this
could make a return at some point.

- **Derived Datatypes** are currently not supported.

- XHC's Reduce currently only supports rank 0 as the root, and will
automatically fall back to another component for other cases.

## Building

This section describes how to compile the XHC component.

XPMEM support in Open MPI is required to reap the full benefits of XHC.
	
- The XHC component will build and work without XPMEM support, but for large
messages (i.e. ones above the CICO threshold) Allreduce/Reduce will be
disabled, and Broadcast will fall-back to less efficient mechanisms.

- XPMEM can be obtained from <https://github.com/hpc/xpmem>, and then
compiled like a common kernel module. You might need to manually point Open
MPI's configure script to XPMEM's installation location, via the
`--with-xpmem=` parameter.

- At run-time, you will need to insert the kernel module and obtain proper
access rights to `/dev/xpmem`.

Apart from instructing Open MPI to include XPMEM support, the rest of the build
process is standard. General information on building Open MPI can be found in
its documentation.

<https://www.open-mpi.org/doc/>  
<https://www.open-mpi.org/faq/?category=building>  
<https://github.com/open-mpi/ompi/blob/master/README.md>

## Running

General information on running Open MPI jobs can be found here:  
<https://www.open-mpi.org/faq/?category=running>  
<https://docs.open-mpi.org/en/v5.0.x/launching-apps/index.html>

`mpirun`'s man page will also be useful:  
<https://docs.open-mpi.org/en/v5.0.x/man-openmpi/man1/mpirun.1.html>

In order for the XHC component to be chosen, its priority must be manually set
higher than other collectives components that implement the same primitives,
via the `coll_xhc_priority` MCA param.

	- Example: `--mca coll_xhc_priority 100`

* Most likely, you will also want the `--bind-to core` param. Otherwise, the
reported process localities might be too general, preventing XHC from correctly
segmenting the system. (MCA `coll_xhc_print_info` will report the generated
hierarchy if you wish to experiment)

### Tuning

* Optional: You might wish to manually specify the topological features that
XHC's hierarchy should conform to. The default is `numa,socket`, which will
group the processes according to NUMA locality and then further group them
according to socket locality. See the `coll_xhc_hierarchy` param.
	
	- Example: `--mca coll_xhc_hierarchy numa,socket`
	- Example: `--mca coll_xhc_hierarchy numa`
	- Example: `--mca coll_xhc_hierarchy flat`
	
	In some systems, small-message Broadcast or the Barrier operation might
	perform better with a flat tree instead of a hierarchical one. Currently,
	manual benchmarking is required to accurately determine this.

* Optional: You might wish to tune XHC's chunk size (default `16K`). Use the
`coll_xhc_chunk_size` param, and try values close to the default and see if
improvements are observed.

	- Example: `--mca coll_xhc_chunk_size 16K`

* Optional: If you wish to focus on latencies of small/medium size messages,
you can try altering the cico-to-zcopy switchover point (MCA
`coll_xhc_cico_max`, default `1K`).
	
	- Example: `--mca coll_xhc_cico_max 1K`

* Optional: If your application is heavy in Broadcast calls and you suspect
that specific ranks might be joining the collective with delay and causing
others to stall waiting for them, try enabling dynamic leadership (MCA
`coll_xhc_dynamic_leader`), and seeing if it makes an improvement. Please let
us know if it does :-).
	
	- Example: `--mca coll_xhc_dynamic_leader 1`

### Example command lines

*Assuming `PATH` and `LD_LIBRARY_PATH` have been set appropriately.*

Default XHC configuration:  
`$ mpirun --mca coll_xhc_priority 100 --bind-to core <application>`

XHC w/ numa-sensitive hierarchy, chunk size @ 16K:  
`$ mpirun --mca coll_xhc_priority 100 --mca coll_xhc_hierarchy numa --mca coll_xhc_chunk_size 16K --bind-to core <application>`

XHC with flat hierarchy (ie. none at all):  
`$ mpirun --mca coll_xhc_priority 100 --mca coll_xhc_hierarchy node [--bind-to core] <application>`

## Benchmarking

This section outlines some tips for benchmarking XHC and intra-node MPI
collectives in general.

### Micro-Benchmarks

For our micro-benchmarking purposes, we have been using [OSU's microbenchmark
suite](https://mvapich.cse.ohio-state.edu/benchmarks/). However, when
micro-benchmarking intra-node collectives, there are some important details
that one needs to look out for.

**CPU Cache** An issue with the OSU micro-benchmarks is that they use the same
buffer for each iteration without altering it. Since modern processors
implicitly cache data, this can lead to false/unrealistic/unrepresentative
results, given that actual real-world applications do not (usually/optimally!)
perform duplicate operations.

Availability of collective operation source data on a processor's local cache
hierarchy will cause certain phenomenons (e.g. slow path memory transactions)
and their effects to remain hidden and undetected in the micro-benchmarking
process, even though they *will* negatively impact performance in actual
applications,

We have created "data-varying" (`_dv` suffix) benchmarks to counter this
problem, which will alter the data before each iteration.

**Microbenchmark's pre-op Barrier** One also needs to be aware how the barrier
that appears before each iteration in the OSU micro-benchmarks affects the
result, especially so when latencies of small messages are concerned. The
underlying implementation of this barrier and the speed/efficiency of its
"release stage" will affect how fast and how synchronized ranks will exit the
barrier, and therefore how fast/synchronized they will enter the benchmarked
collective operation.

For as accurate/clean performance reporting as possible, use a barrier
implementation that has as low a latency as possible. Furthermore, ideally,
all ranks should exit the barrier at the exact same time -- this is more
complex to measure, but can make a difference. In order to have a common
baseline when benchmarking and comparing multiple collectives implementation,
use this same barrier implementation for all benchmark scenarios.

In the environments we tested, XHC's barrier was the best performing one. To
make using this barrier easier, we have put together a small new collective
component, `XB` (= xhc barrier).

XB creates a new nested (duplicate) communicator with a hint to prioritize XHC,
and delegates barrier operations to it. A slightly inconvenient side-effect is
that XHC needs to be on the coll list (MCA `--mca coll`); it doesn't need to
have a high priority, though it can't be less than 0.

* To benchmark Open MPI's `coll/tuned` with XB: `--mca coll basic,libnbc,tuned,xb,xhc --mca coll_xhc_priority 0 --mca coll_xb_priority 95 --mca coll_tuned_priority 90`

* Or XHC itself, with XB: `--mca coll basic,libnbc,xb,xhc --mca coll_xhc_priority 90 --mca coll_xb_priority 95`

It is also possible to specify the hierarchy to be used for XB's barrier (the
request will be passed in string form to XHC, only for the nested communicator)
via the `coll_xb_hierarchy` MCA parameter.

In our fork of the OSU micro-benchmarks, you will also find
"integrity-checking" variants (`_integrity` suffix). These can help verify that
collective operations complete successfully without data corruption.

Our OSU micro-benchmarks fork:  
<https://github.com/CARV-ICS-FORTH/XHC-OpenMPI/tree/xhc-fresh/osu-micro-benchmarks>

The XB component:  
<https://github.com/CARV-ICS-FORTH/XHC-OpenMPI/tree/xhc-fresh/xb>

### Applications

We expect to see any meaningful performance improvement with XHC in actual
applications, only if they spend a non-insignificant percentage of their
runtime in the collective operations that XHC implements: Broadcast, Barrier,
Allreduce, Reduce.

One known such application is [miniAMR](https://github.com/Mantevo/miniAMR).
The application parameters (e.g. the refine count and frequency) will affect
the amount of time spent in the Allreduce primitive.

Another one is Microsoft's [CNTK](https://github.com/microsoft/CNTK), also
heavy in Allreduce, though it actually makes use of the non-blocking
`Iallreduce` variant. However, it can easily be converted to use the blocking
variant instead (contact for patch). Comparing the performance of the
unmodified CNTK with OpenMPI's `coll/libnbc`, versus that of the patched CNTK
with XHC reveals that this modification is sensible and beneficial.

Finally, while we have not yet rigorously evaluated it,
[PiSvM](http://pisvm.sourceforge.net/) is another candidate, with intense use
of MPI Broadcast.

---

Contact: George Katevenis (gkatev@ics.forth.gr), Manolis Ploumidis (ploumid@ics.forth.gr)  
Computer Architecture and VLSI Systems (CARV) Laboratory, ICS Forth
