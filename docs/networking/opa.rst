Omni-Path Architecture (OPA)
============================

How can the multi-rail settings be adjusted if multiple HFI (Host Fabric Interface) cards are installed on the system?
----------------------------------------------------------------------------------------------------------------------

Multi-Rail feature allows a process to use multiple HFIs to transfer a
message to improve message bandwidth. The PSM2 library handles the
support for multi-rail, and is off by default. The multi-rail settings
can be modified using the following environment variables:

* ``PSM2_MULTIRAIL=[0,1,2]``: 0=Disabled, 1=Enable across all HFIs in
  the system, 2=Enable multi-rail within a NUMA node
* ``PSM2_MULTIRAIL_MAP=unit:port,unit:port``

The variables above may be included in the ``mpirun`` command line or
in the environment. For example:

.. code-block::

   shell$ mpirun -mca mtl [psm2|ofi] -x PSM2_MULTIRAIL=1 -n 2 -H host1,host2 ./a.out

.. note:: When using the OFI MTL, please ensure that the PSM2 OFI provider is used for
          communication with OPA devices.

/////////////////////////////////////////////////////////////////////////

What is Multi-HFI support in PSM2 and how does it differ from multi-rail?
-------------------------------------------------------------------------

Multi-HFI support is intended to describe the use of multiple HFIs in
a system among MPI ranks local to a node in order to load-balance the
hardware resources. It differs from Multi-Rail feature which is
intended to allow a single process to use all HFIs in the system. For
an MPI job with multiple ranks in a node, the default PSM2 behavior
depends on the affinity settings of the MPI process. PSM2 defaults to
using the HFI (Host Fabric Interface) that is in the same NUMA node as
that of the MPI process.  Users can restrict access to a single HFI
using the environment variable:

* ``HFI_UNIT=N``: valid values of ``N`` are 0, 1, 2, and 3

More details can be found on the PSM2 Programmer's Guide and Omni-Path
Fabric Performance Tuning Guide. The full documentation `is available
here
<https://www.intel.com/content/www/us/en/support/articles/000016242/network-and-i-o/fabric-products.html>`_.

.. error:: TODO Is this still a correct reference?
