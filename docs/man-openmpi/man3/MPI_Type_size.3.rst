.. _mpi_type_size:


MPI_Type_size
=============

.. include_body

:ref:`MPI_Type_size`, :ref:`MPI_Type_size_x` - Returns the number of bytes
occupied by entries in a data type.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Type_size, MPI_Type_size_x

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_size.rst

INPUT PARAMETER
---------------
* ``datatype``: Datatype (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Datatype size (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_size` returns the total size, in bytes, of the entries in the
type signature associated with datatype; i.e., the total size of the
data in a message that would be created with this datatype. Entries that
occur multiple times in the datatype are counted with their
multiplicity. For either function, if the *size* parameter cannot
express the value to be returned (e.g., if the parameter is too small to
hold the output value), it is set to MPI_UNDEFINED.

NOTE
----

Note that :ref:`MPI_Type_size_x` is  *deprecated* as of MPI-4.1. Please use
the big count version of :ref:`MPI_Type_size` instead.


ERRORS
------

.. include:: ./ERRORS.rst
