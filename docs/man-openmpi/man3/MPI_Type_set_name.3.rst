.. _mpi_type_set_name:


MPI_Type_set_name
=================

.. include_body

:ref:`MPI_Type_set_name` |mdash| Sets the name of a data type.

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_set_name.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``type``: Data type for which the identifier is to be set (handle).

INPUT PARAMETER
---------------
* ``type_name``: The character string remembered as the name (string).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_set_name` associates a printable identifier with an MPI data
type.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_name`
