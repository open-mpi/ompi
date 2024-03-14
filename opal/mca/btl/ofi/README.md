# Design notes on BTL/OFI

This is the RDMA only btl based on OFI Libfabric. The goal is to
enable RDMA with multiple vendor hardware through one interface. Most
of the operations are managed by upper layer (osc/rdma). This BTL is
mostly doing the low level work.

Tested providers: sockets,psm2,ugni

## Component

This BTL is requesting libfabric version 1.5 API and will not support
older versions.

The required capabilities of this BTL is `FI_ATOMIC` and `FI_RMA` with
the endpoint type of `FI_EP_RDM` only. This BTL does NOT support
libfabric provider that requires local memory registration
(`FI_MR_LOCAL`).

BTL/OFI will initialize a module with ONLY the first compatible info
returned from OFI.  This means it will rely on OFI provider to do load
balancing. The support for multiple device might be added later.

The BTL creates only one endpoint and one CQ.

## Memory Registration

Open MPI has a system in place to exchange remote address and always
use the remote virtual address to refer to a piece of memory. However,
some libfabric providers might not support the use of virtual address
and instead will use zero-based offset addressing.

`FI_MR_VIRT_ADDR` is the flag that determine this
behavior. `mca_btl_ofi_reg_mem()` handles this by storing the base
address in registration handle in case of the provider does not
support `FI_MR_VIRT_ADDR`. This base address will be used to calculate
the offset later in RDMA/Atomic operations.

The BTL will try to use the address of registration handle as the
key. However, if the provider supports `FI_MR_PROV_KEY`, it will use
provider provided key. Simply does not care.

The BTL does not register local operand or compare. This is why this
BTL does not support `FI_MR_LOCAL` and will allocate every buffer
before registering. This means `FI_MR_ALLOCATED` is supported. So to
be explicit.

Supported MR mode bits (will work with or without):

* enum:
  * `FI_MR_BASIC`
  * `FI_MR_SCALABLE`
* mode bits:
  * `FI_MR_VIRT_ADDR`
  * `FI_MR_ALLOCATED`
  * `FI_MR_PROV_KEY`

The BTL does NOT support (will not work with):

* `FI_MR_LOCAL`
* `FI_MR_MMU_NOTIFY`
* `FI_MR_RMA_EVENT`
* `FI_MR_ENDPOINT`

Just a reminder, in libfabric API 1.5...
`FI_MR_BASIC == (FI_MR_PROV_KEY | FI_MR_ALLOCATED | FI_MR_VIRT_ADDR)`

## Completions

Every operation in this BTL is asynchronous. The completion handling
will occur in `mca_btl_ofi_component_progress()` where we read the CQ
with the completion context and execute the callback functions. The
completions are local. No remote completion event is generated as
local completion already guarantee global completion.

The BTL keep tracks of number of outstanding operations and provide
flush interface.

## Sockets Provider

Sockets provider is the proof of concept provider for libfabric. It is
supposed to support all the OFI API with emulations. This provider is
considered very slow and bound to raise problems that we might not see
from other faster providers.

Known Problems:

* sockets provider uses progress thread and can cause segfault in
  finalize as we free the resources while progress thread is still
  using it. `sleep(1)` was put in `mca_btl_ofi_component_close()` for
  this reason.
* sockets provider deadlock in two-sided mode. Might be something
  about buffered recv.  (August 2018).

## Scalable Endpoint

This BTL will try to use scalable endpoint to create communication
context. This will increase multithreaded performance for some
application. The default number of context created is 1 and can be
tuned VIA MCA parameter `btl_ofi_num_contexts_per_module`. It is
advised that the number of context should be equal to number of
physical core for optimal performance.

User can disable scalable endpoint by MCA parameter
`btl_ofi_disable_sep`.  With scalable endpoint disabled, the BTL will
alias OFI endpoint to both tx and rx context.

## Two sided communication

Two sided communication is added later on to BTL OFI to enable non
tag-matching provider to be able to use in Open MPI with this
BTL. However, the support is only for "functional" and has not been
optimized for performance at this point. (August 2018)
