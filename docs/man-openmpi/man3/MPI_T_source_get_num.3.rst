.. _mpi_t_source_get_num:


MPI_T_source_get_num
====================

.. include_body

:ref:`MPI_T_source_get_num` |mdash| Query the number of MPI_T event sources

.. The following file was automatically generated
.. include:: ./bindings/mpi_t_source_get_num.rst

OUTPUT PARAMETERS
-----------------

* ``num_sources``: Current number of sources


DESCRIPTION
-----------

:ref:`MPI_T_source_get_num` can be used to query the current number of
MPI_T event sources.

The number of available event sources can be queried with a call to
:ref:`MPI_T_Source_get_num`. An MPI implementation is allowed to
increase the number of sources during the execution of an MPI
process. However, MPI implementations are not allowed to change the
index of an event source or to delete an event source once it has been
made visible to the user (e.g., if new event sources become available
via dynamic loading of additional components in the MPI
implementation).

.. note:: The set of event sources Open MPI exports depends on the platform and
          on MCA parameters. With all built-in producers disabled (the MCA
          parameter ``mca_base_event_register_producers`` set to 0) the count is
          0. Use ``ompi_info --event`` to list the registered sources and event
          types.


ERRORS
------

:ref:`MPI_T_source_get_num` will fail if:

* ``MPI_T_ERR_NOT_INITIALIZED``: The MPI Tools interface is not
  initialized

.. seealso::
   * :ref:`MPI_T_Events`
   * :ref:`MPI_T`
