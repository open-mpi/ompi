.. _mpi_type_get_name:


MPI_Type_get_name
=================

.. include_body

:ref:`MPI_Type_get_name` |mdash| Gets the name of a data type.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_get_name.rst

INPUT PARAMETER
---------------
* ``type``: Data type whose name is to be returned (handle).

OUTPUT PARAMETERS
-----------------
* ``type_name``: The name previously stored on the data type, or an empty string if not such name exists (string).
* ``resultlen``: Length of returned name (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_get_name` returns the printable identifier associated with an
MPI data type.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_set_name`
