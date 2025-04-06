.. _mpi_session_set_errhandler:


MPI_Session_set_errhandler
==========================

.. include_body

:ref:`MPI_Session_set_errhandler` |mdash| Attaches a new error handler to a
session.

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_set_errhandler.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``session``: Session (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for session (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_set_errhandler` attaches a new error handler to a session.
The error handler must be either a predefined error handler or an error
handler created by a call to :ref:`MPI_Session_create_errhandler`.


ERRORS
------

.. include:: ./ERRORS.rst
