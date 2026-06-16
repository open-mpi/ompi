.. _mpi_win_get_name:


MPI_Win_get_name
================

.. include_body

:ref:`MPI_Win_get_name` |mdash| Obtains the name of a window.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_get_name.rst

INPUT PARAMETER
---------------
* ``win``: Window whose name is to be returned (handle).

OUTPUT PARAMETERS
-----------------
* ``win_name``: the name previously stored on the window, or an empty string if no such name exists (string).
* ``resultlen``: Length of returned name (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

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
