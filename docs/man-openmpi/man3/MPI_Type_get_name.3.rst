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

The caller must provide a buffer of at least ``MPI_MAX_OBJECT_NAME`` bytes.
That value depends on which Open MPI interface the application uses: it is
|ompi_max_object_name| bytes for the traditional Open MPI interface (which
Open MPI has used for many years) and |mpi_abi_max_object_name| bytes for
the MPI Forum standard ABI (the value required by the MPI standard).  Open
MPI returns a name (including its terminating null character) that fits
within whichever limit corresponds to the interface the application was
compiled against.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_set_name`
