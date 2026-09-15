OpenFabrics Interfaces (OFI) / Libfabric support
================================================

"OFI" stands for the `OpenFabrics Interfaces
<https://libfabric.org/>`_, which are implemented in the ``libfabric``
library; the two terms are typically used interchangeably.

Open MPI supports many different underlying networks via Libfabric,
including (but not limited to):

* AWS EFA
* Cisco usNIC
* Cornelis Networks Omni-Path
* HPE Slingshot 11

In general, the OFI-based components in Open MPI auto-select themselves
as appropriate at run time.  The remaining sections describe the
OFI-based components and the tuning options for specific OFI-based
network types.

Libfabric (OFI) components in Open MPI
--------------------------------------

Open MPI has three main components for Libfabric (OFI) communication:

#. The ``ofi`` MTL, available since Open MPI v1.10, is used with the
   ``cm`` PML for two-sided MPI communication (for example,
   ``MPI_Send`` and ``MPI_Recv``).  It requires that the Libfabric
   provider support reliable datagrams with ordered tagged messaging
   |mdash| specifically, ``FI_EP_RDM`` endpoints, ``FI_TAGGED``
   capabilities, and ``FI_ORDER_SAS`` ordering.

#. The ``ofi`` BTL, available since Open MPI v4.0.0, is primarily
   intended for one-sided MPI communication (for example,
   ``MPI_Put``), but can also support BTL send/receive operations.  It
   requires that the Libfabric provider support reliable datagrams, RMA
   and atomic operations, and remote atomic completion notifications
   |mdash| specifically, ``FI_EP_RDM`` endpoints, ``FI_RMA`` and
   ``FI_ATOMIC`` capabilities, and the ``FI_DELIVERY_COMPLETE`` op
   flag.

#. The ``usnic`` BTL is used exclusively with Cisco usNIC-based
   networks.  It auto-selects itself over the other OFI-based
   components when run on such networks.

Consult each Libfabric provider's man page (for example,
``fi_sockets(7)``) to understand which provider will work for each of
the components above.  Some providers must be paired with a Libfabric
utility provider; for example, the verbs provider needs the ``ofi_rxm``
utility provider to supply reliable datagram endpoint support
(``verbs;ofi_rxm``).

Each component has MCA parameters that specify the Libfabric
provider(s) to include in or exclude from the selection process.  For
example:

.. code-block::

   shell$ mpirun --mca pml cm --mca mtl ofi \
          --mca mtl_ofi_provider_include psm2 ./mpi_hello

Each component also has its own component-specific parameters; use
``ompi_info`` to list them.  For example:

.. code-block::

   shell$ ompi_info --param mtl ofi --level 9

Provider and domain selection
-----------------------------

Open MPI asks Libfabric for a list of ``fi_info`` records that satisfy the
capabilities required by the selected component.  A record describes a
provider, its fabric, and a domain.  In this context, a Libfabric domain is
the provider's view of a usable network resource; on many systems it
corresponds to one network interface card or one provider-specific network
endpoint.  The domain name is reported in the ``fi_info`` record and is not
necessarily the same as the operating system device name.

The ``ofi`` matching transport layer and the ``ofi`` byte transfer layer use
the common Open MPI selection code.  The selection process is:

#. Open MPI applies the provider include or exclude setting to the records
   returned by Libfabric.  An include list is a whitelist; an exclude list is
   a blacklist.  These settings select provider names, not arbitrary hardware
   device names.
#. Open MPI selects one compatible provider record.  When several records
   describe domains of the same provider with the same requested capabilities,
   those records are candidates for local-resource selection.
#. If the process is not bound to a set of processing units, Open MPI selects
   among the candidate domains using the process's local rank.  This is a
   round-robin choice intended to balance ranks across domains.
#. If the process is bound and the records contain usable PCI information,
   Open MPI calculates device locality and selects a domain closest to the
   process.  If several domains are equally close, the process's rank within
   its processor package selects among the tied domains.
#. If accelerator locality is enabled and the accelerator and provider expose
   usable PCI information, Open MPI first attempts to select the domain closest
   to the accelerator.  Ties are distributed using the local rank on that
   accelerator.
#. If locality information is unavailable, Open MPI falls back to the rank-based
   round-robin choice.  A process that is not bound cannot receive a locality
   guarantee from the operating system.

This selection is performed independently by each MPI process.  It does not
mean that every process opens every domain, and it does not guarantee that a
particular operating system interface is selected.  The provider's ``fi_info``
records, process binding, PCI topology information, and the selected Open MPI
component all affect the result.  Open MPI then opens the selected Libfabric
fabric and domain and creates its endpoints and completion resources.

The common selection parameter is exposed through component-specific aliases.
For example, these commands show the provider and selection diagnostics for
the matching transport layer and the byte transfer layer:

.. code-block:: sh

   shell$ mpirun --mca mtl_ofi_verbose 1 \
          --mca mtl_ofi_provider_include cxi ./mpi_hello
   shell$ mpirun --mca btl_ofi_verbose 1 \
          --mca btl_ofi_provider_include cxi ./mpi_hello

The exact diagnostics and available component parameters depend on the Open
MPI and Libfabric versions in use.  ``ompi_info`` is the authoritative way to
list the parameters in a particular installation.

LINKx provider
--------------

Libfabric also provides a provider named ``lnx`` (LINKx).  LINKx can combine
multiple Libfabric providers or domains behind one tagged-message endpoint. It
is an optional Libfabric feature, not an Open MPI-specific network component.
Whether it is appropriate depends on the provider versions, the system
topology, and the performance goals of the deployment.  HPE or the site
administrator should confirm that it is recommended for a particular
Slingshot installation before it is enabled.

The provider must be configured through the Libfabric environment variable
``FI_LNX_PROV_LINKS``.  For example, the following requests one LINKx group
containing shared memory and two Slingshot domains:

.. code-block:: sh

   shell$ export FI_LNX_PROV_LINKS="shm+cxi:cxi0,cxi1"
   shell$ mpirun --mca pml cm --mca mtl ofi \
          --mca mtl_ofi_provider_include lnx ./mpi_hello

The provider manual describes additional forms, including multiple groups and
provider-specific domain lists.  All nodes in the MPI job must use compatible
LINKx configurations and the same ordering of linked providers and domains.
If shared memory is included, LINKx uses it for intra-node communication;
other linked providers are used for off-node communication.  LINKx can also
distribute messages across multiple linked domains, according to its
multi-rail selection policy.

There are important tradeoffs.  The LINKx provider described by the current
Libfabric manual supports tagged operations, but does not provide hardware
offload such as hardware tag matching.  Because memory registration does not
identify the eventual operation or destination, LINKx registers memory with
all linked providers.  This can increase registration cost and may affect
memory behavior.  Measure an application with and without LINKx before making
it a site-wide default.

GPU memory and Libfabric providers
----------------------------------

When an MPI buffer resides in graphics processing unit memory, Open MPI can
request Libfabric support for heterogeneous memory through the ``FI_HMEM``
capability.  The ``ofi`` matching transport layer and byte transfer layer
   request this capability by default when it is available in the build.  The
   matching transport layer and byte transfer layer can disable the request
   with ``mtl_ofi_disable_hmem`` and ``btl_ofi_disable_hmem``, respectively.

Requesting heterogeneous-memory support does not guarantee direct network
access to every type of GPU memory.  The selected provider must advertise the
required capability, and the Libfabric build must include support for the GPU
runtime in use.  If no suitable provider is found with heterogeneous-memory
requirements, Open MPI may retry provider discovery without that requirement;
this can result in host-memory staging or a provider that does not support
direct GPU buffers.

LINKx deserves additional care for GPU buffers.  LINKx forwards memory
registration to all providers in a link, and the linked providers may have
different GPU-memory capabilities.  A LINKx configuration should therefore
contain only providers that support the intended buffer type and memory
registration mode.  Validate the configuration with a representative
application and consult the installed provider manuals before using GPU
buffers in production.

.. important:: When using the HPE CXI provider with ``mpirun`` as the
               job launcher, it is recommended to set the PRRTE
               ``ras_base_launch_orted_on_hn`` MCA parameter to 1 by
               adding ``--prtemca ras_base_launch_orted_on_hn 1`` to
               the job launch command line.  This ensures that MPI
               processes launched on the first node of an allocation
               are able to use the CXI provider.

For more information, refer to the `Libfabric web site
<https://libfabric.org/>`_.

Omni-Path: multi-rail with multiple HFI cards
---------------------------------------------

The multi-rail feature lets a single process use multiple Host Fabric
Interface (HFI) cards to transfer a message, improving message
bandwidth.  The PSM2 library provides multi-rail support, which is off
by default.  The behavior is controlled with the following environment
variables:

* ``PSM2_MULTIRAIL=[0,1,2]``: 0 disables multi-rail, 1 enables it
  across all HFIs in the system, and 2 enables multi-rail within a NUMA
  node.
* ``PSM2_MULTIRAIL_MAP=unit:port,unit:port,...``

These variables may be set on the ``mpirun`` command line or in the
environment.  For example:

.. code-block::

   shell$ mpirun --mca mtl [psm2|ofi] -x PSM2_MULTIRAIL=1 \
          -n 2 -H host1,host2 ./a.out

.. note:: When using the OFI MTL, ensure that the PSM2 OFI provider is
          used for communication with OPA devices.

Omni-Path: multi-HFI support in PSM2
------------------------------------

Multi-HFI support describes the use of multiple HFIs in a system among
the MPI ranks local to a node, in order to load-balance the hardware
resources.  It differs from the multi-rail feature, which allows a
single process to use all HFIs in the system.  For an MPI job with
multiple processes on a single node, the default PSM2 behavior depends
on the affinity settings of the MPI process: the PSM2 library defaults
to using the HFI that is in the same NUMA node as the MPI process.

Users can restrict a process to a single HFI with an environment
variable:

* ``HFI_UNIT=N``: valid values of ``N`` are 0, 1, 2, and 3.

More details can be found in the PSM2 Programmer's Guide and the
Omni-Path Fabric Performance Tuning Guide; see the `Cornelis Networks
Customer Center <https://customercenter.cornelisnetworks.com/>`_.
