.. _mpi_info_dup:


MPI_Info_dup
============

.. include_body

:ref:`MPI_Info_dup` |mdash| Duplicates an info object.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_dup.rst

INPUT PARAMETER
---------------
* ``info``: Info object (handle).

OUTPUT PARAMETERS
-----------------
* ``newinfo``: Info object (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_dup` duplicates an existing info object, creating a new object,
with the same (key,value) pairs and the same ordering of keys.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_delete`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_set`
