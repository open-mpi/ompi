MPI allows applications to pass ``MPI_Info`` hints to
:ref:`MPI_File_open`, :ref:`MPI_File_set_view`, and
:ref:`MPI_File_set_info`.  Hints are advisory: OMPIO may ignore hints
that are not supported in the selected component stack.  The
:ref:`MPI_File_get_info` routine returns a new ``MPI_Info`` object
containing the public hints that OMPIO currently associates with the
file.  The caller is responsible for freeing that returned object with
:ref:`MPI_Info_free`.

OMPIO preserves the spelling of a supported hint that was accepted
from the user's ``MPI_Info`` object.  When OMPIO reports an internal
default that did not come from a user-supplied hint, it uses the
canonical public spelling shown below.

OMPIO common hints
^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Hint
     - Accepted by
     - Default / MCA parameter
     - Notes
   * - ``cb_buffer_size``
     - ``MPI_File_open``, ``MPI_File_set_info``,
       ``MPI_File_set_view``
     - ``io_ompio_bytes_per_agg``
     - Size, in bytes, of the temporary collective I/O buffer on each
       aggregator.
   * - ``cb_nodes``
     - ``MPI_File_open``, ``MPI_File_set_info``,
       ``MPI_File_set_view``
     - ``io_ompio_num_aggregators`` when that MCA parameter is set to
       a non-negative value.
     - Number of collective I/O aggregators.  When OMPIO is using its
       automatic aggregator selection, this hint is not returned as
       ``-1``.
   * - ``collective_buffering``
     - ``MPI_File_open``, ``MPI_File_set_info``,
       ``MPI_File_set_view``
     - ``true``
     - Controls whether collective buffering is enabled for collective
       I/O decisions.

``sharedfp`` component hints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Component
     - Hint
     - Accepted by
     - Default / MCA parameter
     - Notes
   * - ``sharedfp/individual``
     - ``OMPIO_SHAREDFP_RELAXED_ORDERING``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - If present on a write-capable file open, this hint raises the
       priority of the ``individual`` shared file pointer component.
       OMPIO reports it only when that component is selected for the
       file.

Lustre ``fs`` component hints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Lustre layout hints affect file layout creation.  Lustre applies
these settings only when a file is created in a mode where layout can
be set safely; changing them after a file has been opened does not
change the already-created file layout.

.. list-table::
   :header-rows: 1

   * - Canonical hint
     - Accepted alias
     - Accepted by
     - Default / MCA parameter
     - Notes
   * - ``striping_unit``
     - ``stripe_size``
     - ``MPI_File_open``
     - ``fs_lustre_stripe_size`` when greater than zero.
     - Stripe size in bytes.
   * - ``striping_factor``
     - ``stripe_width``
     - ``MPI_File_open``
     - ``fs_lustre_stripe_width`` when greater than zero.
     - Number of Lustre object storage targets to stripe across.

If an application supplies an alias, :ref:`MPI_File_get_info` returns
the alias spelling.  If both a canonical hint and its alias are
supplied, OMPIO prefers the canonical spelling.  If only an MCA
parameter supplies the value, OMPIO reports the canonical spelling.

GPFS ``fs`` component hints
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GPFS component accepts the hints listed below when the GPFS
component is built and selected.  These hints are consumed when the
file is opened.  Later ``MPI_File_set_info`` or ``MPI_File_set_view``
calls do not reapply them to GPFS, and :ref:`MPI_File_get_info`
continues to report the open-time accepted values.

.. list-table::
   :header-rows: 1

   * - Hint
     - Accepted by
     - Default / MCA parameter
     - Notes
   * - ``useSIOXLib``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Enables optional SIOX-assisted GPFS I/O selection when SIOX
       support is compiled in.
   * - ``gpfsAccessRange``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Passed to GPFS as an access range hint.
   * - ``gpfsFreeRange``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Passed to GPFS as a free range hint.
   * - ``gpfsClearFileCache``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Requests GPFS file-cache clearing.
   * - ``gpfsCancelHints``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Requests cancellation of GPFS hints.
   * - ``gpfsSetReplication``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Passed to GPFS as a replication hint.
   * - ``gpfsByteRange``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Passed to GPFS as a byte range hint.
   * - ``gpfsRestripeData``
     - ``MPI_File_open``
     - No default; no corresponding MCA parameter.
     - Requests GPFS data restriping.

When SIOX support is compiled in, the GPFS component also accepts:
``sioxAccessRange``, ``sioxFreeRange``, ``sioxClearFileCache``,
``sioxCancelHints``, ``sioxDataShipStart``, ``sioxDataShipStop``,
``sioxSetReplication``, ``sioxByteRange``, and
``sioxRestripeData``.
