.. _mpi_type_get_value_index:


MPI_Type_get_value_index
========================

.. include_body

:ref:`MPI_Type_get_value_index` |mdash| Returns a reference (handle) to one of the predefined
datatypes suitable for the use with MPI_MINLOC and MPI_MAXLOC if such predefined type
exists.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_get_value_index.rst

INPUT PARAMETERS
----------------
* ``value_type``: Datatype of the value in pair (handle)
* ``index_type``: Datatype of the index in pair (handle)

OUTPUT PARAMETERS
-----------------
* ``pair_type``: Datatype of the value-index pair (handle)
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_get_value_index` Returns a reference (handle) to one of the predefined
datatypes suitable for the use with MPI_MINLOC and MPI_MAXLOC if such predefined type
exists.

ERRORS
------

.. include:: ./ERRORS.rst
