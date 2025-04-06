.. _mpi_info_delete:


MPI_Info_delete
===============

.. include_body

:ref:`MPI_Info_delete` |mdash| Deletes a key/value pair from *info*.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_delete.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``info``: Info object (handle).

INPUT PARAMETER
---------------
* ``key``: Key (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_delete` deletes a (key,value) pair from *info*. If *key* is not
defined in *info*, the call raises an error of class MPI_ERR_INFO_NOKEY.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_dup`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_set`
