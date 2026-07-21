.. _mpi_get_hw_resource_info:


MPI_Get_hw_resource_info
========================

.. include_body

:ref:`MPI_Get_hw_resource_info` |mdash| Returns information about the hardware
resources on which the calling process can execute.

.. The following file was automatically generated
.. include:: ./bindings/mpi_get_hw_resource_info.rst

OUTPUT PARAMETERS
-----------------
* ``info``: Info object containing local hardware resource information
  (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Get_hw_resource_info` returns an info object describing hardware
resource types associated with the calling \MPI/ process at the moment of the
call. The application is responsible for freeing the returned info object with
:ref:`MPI_Info_free`.

Open MPI obtains this information from hwloc. Each key uses the URI form
``hwloc://<resource-type>``. The value is ``true`` if the calling process is
restricted to a single instance of that resource type and ``false`` if its CPU
binding spans multiple instances. Depending on the local topology, the returned
keys can include:

* ``hwloc://NUMANode``
* ``hwloc://Package``
* ``hwloc://L3Cache``
* ``hwloc://L2Cache``
* ``hwloc://L1Cache``
* ``hwloc://Core``
* ``hwloc://PU``

Resource types absent from the local topology are omitted. Open MPI returns an
empty info object if hardware topology or process binding information is not
available.

The returned keys can be passed as values of the ``mpi_hw_resource_type`` info
key to :ref:`MPI_Comm_split_type` with ``MPI_COMM_TYPE_HW_GUIDED`` or
``MPI_COMM_TYPE_RESOURCE_GUIDED``. For example:

.. code-block:: c

   MPI_Info split_info;
   MPI_Comm resource_comm;

   MPI_Info_create(&split_info);
   MPI_Info_set(split_info, "mpi_hw_resource_type", "hwloc://NUMANode");
   MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_RESOURCE_GUIDED,
                       0, split_info, &resource_comm);
   MPI_Info_free(&split_info);

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Comm_split_type`
