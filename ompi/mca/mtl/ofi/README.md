# Open MPI OFI MTL

The OFI MTL supports Libfabric (a.k.a., [Open Fabrics Interfaces
OFI](https://ofiwg.github.io/libfabric/)) tagged APIs
(`fi_tagged(3)`). At initialization time, the MTL queries libfabric
for providers supporting tag matching (`fi_getinfo(3)`). Libfabric
will return a list of providers that satisfy the requested
capabilities, having the most performant one at the top of the list.
The user may modify the OFI provider selection with mca parameters
`mtl_ofi_provider_include` or `mtl_ofi_provider_exclude`.

## PROGRESS

The MTL registers a progress function to `opal_progress`. There is
currently no support for asynchronous progress. The progress function
reads multiple events from the OFI provider Completion Queue (CQ) per
iteration (defaults to 100, can be modified with the mca
`mtl_ofi_progress_event_cnt`) and iterates until the completion queue is
drained.

## COMPLETIONS

Each operation uses a request type `ompi_mtl_ofi_request_t` which
includes a reference to an operation specific completion callback, an
MPI request, and a context. The context (`fi_context`) is used to map
completion events with `MPI_requests` when reading the CQ.

## OFI TAG

MPI needs to send 96 bits of information per message (32 bits
communicator id, 32 bits source rank, 32 bits MPI tag) but OFI only
offers 64 bits tags. In addition, the OFI MTL uses 2 bits of the OFI
tag for the synchronous send protocol.  Therefore, there are only 62
bits available in the OFI tag for message usage. The OFI MTL offers
the `mtl_ofi_tag_mode` mca parameter with 4 modes to address this:

* `auto` (Default):
  After the OFI provider is selected, a runtime check is performed to
  assess `FI_REMOTE_CQ_DATA` and `FI_DIRECTED_RECV` support (see
  `fi_tagged(3)`, `fi_msg(2)` and `fi_getinfo(3)`). If supported,
  `ofi_tag_full` is used. If not supported, fall back to `ofi_tag_1`.

* `ofi_tag_1`:
  For providers that do not support `FI_REMOTE_CQ_DATA`, the OFI MTL
  will trim the fields (Communicator ID, Source Rank, MPI tag) to make
  them fit the 62 bits available bit in the OFI tag. There are two
  options available with different number of bits for the Communicator
  ID and MPI tag fields. This tag distribution offers: 12 bits for
  Communicator ID (max Communicator ID 4,095) subject to provider
  reserved bits (see `mem_tag_format` below), 18 bits for Source Rank
  (max Source Rank 262,143), 32 bits for MPI tag (max MPI tag is
  `INT_MAX`).

* `ofi_tag_2`:
  Same as 2 `ofi_tag_1` but offering a different OFI tag distribution
  for applications that may require a greater number of supported
  Communicators at the expense of fewer MPI tag bits. This tag
  distribution offers: 24 bits for Communicator ID (max Communicator
  ED 16,777,215. See mem_tag_format below), 18 bits for Source Rank
  (max Source Rank 262,143), 20 bits for MPI tag (max MPI tag
  524,287).

* `ofi_tag_full`:
  For executions that cannot accept trimming source rank or MPI tag,
  this mode sends source rank for each message in the CQ DATA. The
  Source Rank is made available at the remote process CQ
  (`FI_CQ_FORMAT_TAGGED` is used, see `fi_cq(3)`) at the completion of
  the matching receive operation. Since the minimum size for
  `FI_REMOTE_CQ_DATA` is 32 bits, the Source Rank fits with no
  limitations. The OFI tag is used for the Communicator id (28 bits,
  max Communicator ID 268,435,455. See `mem_tag_format` below), and
  the MPI tag (max MPI tag is `INT_MAX`). If this mode is selected by
  the user and `FI_REMOTE_CQ_DATA` or `FI_DIRECTED_RECV` are not
  supported, the execution will abort.

* `mem_tag_format` (`fi_endpoint(3)`)
  Some providers can reserve the higher order bits from the OFI tag
  for internal purposes.  This is signaled in `mem_tag_format` (see
  `fi_endpoint(3)`) by setting higher order bits to zero. In such
  cases, the OFI MTL will reduce the number of communicator ids
  supported by reducing the bits available for the communicator ID
  field in the OFI tag.

## SCALABLE ENDPOINTS

OFI MTL supports OFI Scalable Endpoints (SEP) feature as a means to
improve multi-threaded application throughput and message
rate. Currently the feature is designed to utilize multiple TX/RX
contexts exposed by the OFI provider in conjunction with a
multi-communicator MPI application model. Therefore, new OFI contexts
are created as and when communicators are duplicated in a lazy fashion
instead of creating them all at once during init time and this
approach also favours only creating as many contexts as needed.

1. Multi-communicator model:
   With this approach, the MPI application is required to first duplicate
   the communicators it wants to use with MPI operations (ideally creating
   as many communicators as the number of threads it wants to use to call
   into MPI). The duplicated communicators are then used by the
   corresponding threads to perform MPI operations. A possible usage
   scenario could be in an MPI + OMP application as follows
   (example limited to 2 ranks):

    ```c
    MPI_Comm dup_comm[n];
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    for (i = 0; i < n; i++) {
        MPI_Comm_dup(MPI_COMM_WORLD, &dup_comm[i]);
    }
    if (rank == 0) {
    #pragma omp parallel for private(host_sbuf, host_rbuf) num_threads(n)
        for (i = 0; i < n ; i++) {
            MPI_Send(host_sbuf, MYBUFSIZE, MPI_CHAR,
                     1, MSG_TAG, dup_comm[i]);
            MPI_Recv(host_rbuf, MYBUFSIZE, MPI_CHAR,
                     1, MSG_TAG, dup_comm[i], &status);
        }
    } else if (rank == 1) {
    #pragma omp parallel for private(status, host_sbuf, host_rbuf) num_threads(n)
        for (i = 0; i < n ; i++) {
            MPI_Recv(host_rbuf, MYBUFSIZE, MPI_CHAR,
                     0, MSG_TAG, dup_comm[i], &status);
            MPI_Send(host_sbuf, MYBUFSIZE, MPI_CHAR,
                     0, MSG_TAG, dup_comm[i]);
        }
    }
    ```

2. MCA variables:
   To utilize the feature, the following MCA variables need to be set:

   * `mtl_ofi_enable_sep`:
     This MCA variable needs to be set to enable the use of Scalable
     Endpoints (SEP) feature in the OFI MTL. The underlying provider
     is also checked to ensure the feature is supported. If the
     provider chosen does not support it, user needs to either set
     this variable to 0 or select a different provider which supports
     the feature.  For single-threaded applications one OFI context is
     sufficient, so OFI SEPs may not add benefit.  Note that
     `mtl_ofi_thread_grouping` (see below) needs to be enabled to use
     the different OFI SEP contexts. Otherwise, only one context (ctxt
     0) will be used.

     Default: 0

     Command-line syntax: `--mca mtl_ofi_enable_sep 1`

   * `mtl_ofi_thread_grouping`:
     Turn Thread Grouping feature on. This is needed to use the
     Multi-communicator model explained above. This means that the OFI
     MTL will use the communicator ID to decide the SEP contexts to be
     used by the thread. In this way, each thread will have direct
     access to different OFI resources. If disabled, only context 0
     will be used.  Requires `mtl_ofi_enable_sep` to be set to 1.

     Default: 0

     It is not recommended to set the MCA variable for:

     * Multi-threaded MPI applications not following multi-communicator
       approach.
     * Applications that have multiple threads using a single
       communicator as it may degrade performance.

     Command-line syntax: `--mca mtl_ofi_thread_grouping 1`

   * `mtl_ofi_num_ctxts`:
     This MCA variable allows user to set the number of OFI SEP
     contexts the application expects to use. For multi-threaded
     applications using Thread Grouping feature, this number should be
     set to the number of user threads that will call into MPI. This
     variable will only have effect if `mtl_ofi_enable_sep` is set to 1.

     Default: 1

     Command-line syntax: `--mca mtl_ofi_num_ctxts N` (`N`: number of OFI contexts required by application)

3. Notes on performance:
   * OFI MTL will create as many TX/RX contexts as set by MCA
     mtl_ofi_num_ctxts.  The number of contexts that can be created is
     also limited by the underlying provider as each provider may have
     different thresholds. Once the threshold is exceeded, contexts are
     used in a round-robin fashion which leads to resource sharing
     among threads. Therefore locks are required to guard against race
     conditions. For performance, it is recommended to have

       Number of threads = Number of communicators = Number of contexts

     For example, when using PSM2 provider, the number of contexts is
     dictated by the Intel Omni-Path HFI1 driver module.

   * OPAL layer allows for multiple threads to enter progress
     simultaneously. To enable this feature, user needs to set MCA
     variable `max_thread_in_progress`. When using Thread Grouping
     feature, it is recommended to set this MCA parameter to the number
     of threads expected to call into MPI as it provides performance
     benefits.

     Default: 1

     Command-line syntax: `--mca opal_max_thread_in_progress N` (`N`: number of threads expected to make MPI calls )

   * For applications using a single thread with multiple communicators
     and MCA variable `mtl_ofi_thread_grouping` set to 1, the MTL will
     use multiple contexts, but the benefits may be negligible as only
     one thread is driving progress.

## SPECIALIZED FUNCTIONS

To improve performance when calling message passing APIs in the OFI
mtl specialized functions are generated at compile time that eliminate
all the if conditionals that can be determined at init and don't need
to be queried again during the critical path. These functions are
generated by perl scripts during make which generate functions and
symbols for every combination of flags for each function.

1. ADDING NEW FLAGS FOR SPECIALIZATION OF EXISTING FUNCTION:
   To add a new flag to an existing specialized function for handling
   cases where different OFI providers may or may not support a
   particular feature, then you must follow these steps:

   1. Update the `_generic` function in `mtl_ofi.h` with the new flag
      and the if conditionals to read the new value.
   1. Update the `*.pm` file corresponding to the function with the
      new flag in: `gen_funcs()`, `gen_*_function()`, &
      `gen_*_sym_init()`
   1. Update `mtl_ofi_opt.h` with:
      * The new flag as `#define NEW_FLAG_TYPES #NUMBER_OF_STATES`.
        Example: #define OFI_CQ_DATA 2 (only has TRUE/FALSE states)
      * Update the function's types with:
        `#define OMPI_MTL_OFI_FUNCTION_TYPES [NEW_FLAG_TYPES]`

1. ADDING A NEW FUNCTION FOR SPECIALIZATION:
   To add a new function to be specialized you must
   follow these steps:
   1. Create a new `mtl_ofi_<function_name>_opt.pm` based off
      `opt_common/mtl_ofi_opt.pm.template`
   1. Add new `.pm` file to `generated_source_modules` in `Makefile.am`
   1. Add `.c` file to `generated_sources` in `Makefile.am` named the
      same as the corresponding `.pm` file
   1. Update existing or create function in `mtl_ofi.h` to `_generic`
      with new flags.
   1. Update `mtl_ofi_opt.h` with:
      1. New function types: `#define OMPI_MTL_OFI_FUNCTION_TYPES` `[FLAG_TYPES]`
      1. Add new function to the `struct ompi_mtl_ofi_symtable`:
         ```c
         struct ompi_mtl_ofi_symtable {
               ...
               int (*ompi_mtl_ofi_FUNCTION OMPI_MTL_OFI_FUNCTION_TYPES )
         }
         ```
      1. Add new symbol table init function definition:
         ```c
         void ompi_mtl_ofi_FUNCTION_symtable_init(struct ompi_mtl_ofi_symtable* sym_table);
         ```
   1. Add calls to init the new function in the symbol table and
      assign the function pointer to be used based off the flags in
      `mtl_ofi_component.c`:
      * `ompi_mtl_ofi_FUNCTION_symtable_init(&ompi_mtl_ofi.sym_table);`
      * `ompi_mtl_ofi.base.mtl_FUNCTION = ompi_mtl_ofi.sym_table.ompi_mtl_ofi_FUNCTION[ompi_mtl_ofi.flag];`

## EXAMPLE SPECIALIZED FILE

The code below is an example of what is generated by the
specialization scripts for use in the OFI mtl. This code specializes
the blocking send functionality based on `FI_REMOTE_CQ_DATA` & OFI
Scalable Endpoint support provided by an OFI Provider. Only one
function and symbol is used during runtime based on if
`FI_REMOTE_CQ_DATA` is supported and/or if OFI Scalable Endpoint support
is enabled.

```c
/*
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mtl_ofi.h"

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_false_false(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode)
{
    const bool OFI_CQ_DATA = false;
    const bool OFI_SCEP_EPS = false;

    return ompi_mtl_ofi_send_generic(mtl, comm, dest, tag,
                                    convertor, mode,
                                    OFI_CQ_DATA, OFI_SCEP_EPS);
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_false_true(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode)
{
    const bool OFI_CQ_DATA = false;
    const bool OFI_SCEP_EPS = true;

    return ompi_mtl_ofi_send_generic(mtl, comm, dest, tag,
                                    convertor, mode,
                                    OFI_CQ_DATA, OFI_SCEP_EPS);
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_true_false(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode)
{
    const bool OFI_CQ_DATA = true;
    const bool OFI_SCEP_EPS = false;

    return ompi_mtl_ofi_send_generic(mtl, comm, dest, tag,
                                    convertor, mode,
                                    OFI_CQ_DATA, OFI_SCEP_EPS);
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_true_true(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode)
{
    const bool OFI_CQ_DATA = true;
    const bool OFI_SCEP_EPS = true;

    return ompi_mtl_ofi_send_generic(mtl, comm, dest, tag,
                                    convertor, mode,
                                    OFI_CQ_DATA, OFI_SCEP_EPS);
}

void ompi_mtl_ofi_send_symtable_init(struct ompi_mtl_ofi_symtable* sym_table)
{

    sym_table->ompi_mtl_ofi_send[false][false]
        = ompi_mtl_ofi_send_false_false;


    sym_table->ompi_mtl_ofi_send[false][true]
        = ompi_mtl_ofi_send_false_true;


    sym_table->ompi_mtl_ofi_send[true][false]
        = ompi_mtl_ofi_send_true_false;


    sym_table->ompi_mtl_ofi_send[true][true]
        = ompi_mtl_ofi_send_true_true;

}
```
