.. _mpi_comm_set_errhandler:


MPI_Comm_set_errhandler
=======================

.. include_body

:ref:`MPI_Comm_set_errhandler` |mdash| Attaches a new error handler to a
communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_set_errhandler.rst

INPUT/OUTPUT PARAMETER
----------------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``errhandler``: New error handler for communicator (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_set_errhandler` attaches a new error handler to a communicator.
The error handler must be either a predefined error handler or an error
handler created by a call to :ref:`MPI_Comm_create_errhandler`. This call is
identical to :ref:`MPI_Errhandler_set`, the use of which is deprecated.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_errhandler`
   * :ref:`MPI_Comm_get_errhandler`
