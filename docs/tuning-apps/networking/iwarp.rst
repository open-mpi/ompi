iWARP Support
=============

Open MPI's support for iWARP devices has changed over time.

In the Open MPI |ompi_series| series, iWARP devices are
supported via the OFI (``ofi``) MTL via the CM (``cm``) PML.

.. note:: Prior versions of Open MPI supported iWARP devies via the
          ``openib`` BTL.  Open MPI |ompi_series| no longer includes
          the ``openib`` BTL.

Specifically, iWARP support is provides through the ``rxm`` provider
of the OpenFabrics Interfaces library ``libfabric``.  There is
software emulation involved in the MPI support of iWARP devices (and
therefore at least some level of performance degradation), but the
current iWARP vendors have chosen not to provide a higher-performance
option.

.. important:: iWARP support is not well tested or maintained in Open
               MPI.  The Open MPI community would love to have a
               maintainer who can develop and provide support for
               iWARP devices over time.
