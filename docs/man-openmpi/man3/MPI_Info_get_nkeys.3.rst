.. _mpi_info_get_nkeys:


MPI_Info_get_nkeys
==================

.. include_body

:ref:`MPI_Info_get_nkeys` |mdash| Gets the number of keys currently defined in an
info object.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_get_nkeys.rst

INPUT PARAMETER
---------------
* ``info``: Info object (handle).

OUTPUT PARAMETERS
-----------------
* ``nkeys``: Number of defined keys (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_get_nkeys` returns the number of currently defined keys in
*info*.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_get_nthkey`
   * :ref:`MPI_Info_get_valuelen`
