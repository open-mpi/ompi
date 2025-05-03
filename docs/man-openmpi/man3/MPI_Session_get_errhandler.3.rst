.. _mpi_session_get_errhandler:


MPI_Session_get_errhandler
==========================

.. include_body

:ref:`MPI_Session_get_errhandler` |mdash| Retrieves error handler associated with a
session.

.. The following file was automatically generated
.. include:: ./bindings/mpi_session_get_errhandler.rst

INPUT PARAMETER
---------------
* ``session``: Session (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for session (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Session_get_errhandler` retrieves the error handler currently associated
with a session.


ERRORS
------

.. include:: ./ERRORS.rst
