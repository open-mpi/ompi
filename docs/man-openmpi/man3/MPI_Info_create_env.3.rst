.. _mpi_info_create_env:


MPI_Info_create_env
===================

.. include_body

:ref:`MPI_Info_create_env` |mdash| Creates a new info object with the same construction as :ref:`MPI_INFO_ENV` as created during :ref:`MPI_Init`  or :ref:`MPI_Init_thread` when the same arguments
are used.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_create_env.rst

OUTPUT PARAMETERS
-----------------
* ``info``: Info object created (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_create_env` creates a new info object with the same construction as :ref:`MPI_INFO_ENV` as created during :ref:`MPI_Init` or :ref:`MPI_Init_thread` when the same arguments are used.

NOTE
----

:ref:`MPI_Info_create_env` is one of the few functions that can be called
before :ref:`MPI_Init` and after :ref:`MPI_Finalize`.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_delete`
   * :ref:`MPI_Info_dup`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_set`
