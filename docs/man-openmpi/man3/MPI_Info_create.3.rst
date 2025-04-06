.. _mpi_info_create:


MPI_Info_create
===============

.. include_body

:ref:`MPI_Info_create` |mdash| Creates a new info object.

.. The following file was automatically generated
.. include:: ./bindings/mpi_info_create.rst

OUTPUT PARAMETERS
-----------------
* ``info``: Info object created (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Info_create` creates a new info object. The newly created object
contains no key/value pairs.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Info_delete`
   * :ref:`MPI_Info_dup`
   * :ref:`MPI_Info_free`
   * :ref:`MPI_Info_get`
   * :ref:`MPI_Info_set`
