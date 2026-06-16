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

The length of the name that can be stored depends on which Open MPI
interface the application uses.  The traditional Open MPI interface limits
names to |ompi_max_object_name| bytes (the value of ``MPI_MAX_OBJECT_NAME``
in ``mpi.h``, which Open MPI has used for many years), while the MPI Forum
standard ABI limits names to |mpi_abi_max_object_name| bytes (the value
required by the MPI standard).  Open MPI honors whichever limit corresponds
to the interface the application was compiled against.  A name longer than
the applicable limit (including its terminating null character) is silently
truncated; setting an over-long name is not an error.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_name`
