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

.. important:: When using the HPE CXI provider with ``mpirun`` as the
               job launcher, it is recommended to set the PRRTE
               ``ras_base_launch_orted_on_hn`` MCA parameter to 1 by
               adding ``--prtemca ras_base_launch_orted_on_hn 1`` to
               the job launch command line.  This ensures that MPI
               processes launched on the first node of an allocation
               are able to use the CXI provider.

For more information, refer to the `Libfabric web site
<https://libfabric.org/>`_.

OFI BTL: multi-rail / multi-NIC support
---------------------------------------

Starting with Open MPI v6.0.0, the ``ofi`` BTL can use multiple
Libfabric NICs per process when more than one NIC is available for the
selected provider.  This primarily benefits ``ob1``-based traffic
patterns, allowing a single MPI rank to spread its traffic across
multiple NICs instead of being limited to the bandwidth of a single
device.

Open MPI first counts the distinct physical NICs the selected provider
reports on the local node, ignoring duplicates (some providers report
the same physical NIC more than once), and lists them in a fixed order
so that every process on the node agrees on that order.  It then gives
each process an even share of those NICs, computed automatically as
roughly the number of NICs divided by the number of processes on the
node (always at least one NIC per process).

Each process is assigned a different portion of the NIC list, so when
there are at least as many NICs as processes on the node, the processes
use non-overlapping sets of NICs rather than all competing for the same
device.  On nodes with many NICs, this can improve both the bandwidth
seen by a single rank and the total bandwidth of the node.

This behavior is automatic; there is no MCA parameter to manually assign
per-rank NIC slices.  The ``ofi`` BTL still selects Libfabric providers
through its normal provider include / exclude MCA parameters.

For example, to force ``ob1`` to use the ``ofi`` BTL together with the
usual loopback support:

.. code-block:: sh

   shell$ mpirun --mca pml ob1 --mca btl ofi,self ./mpi_hello

On multi-NIC Libfabric systems such as AWS EFA instances, this lets a
single rank's traffic use multiple NICs, and also spreads the processes
on a node across different NICs.

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
