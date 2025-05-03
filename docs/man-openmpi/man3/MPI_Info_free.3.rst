.. _mpi_info_free:


MPI_Info_free
=============

.. include_body

:ref:`MPI_Info_free` |mdash| Frees an info object.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_free.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``info``: Info object (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_free` frees *info* and sets it to MPI_INFO_NULL.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_delete`
   * :ref:`MPI_Info_dup`
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_set`
