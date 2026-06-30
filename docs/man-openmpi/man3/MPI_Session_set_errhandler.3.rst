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


NOTES
-----

When using MPI_ERRORS_ABORT with a session, note that per MPI-5.0 Sec 9.3
(pp.447-448), this error handler aborts ONLY the local MPI process, not the
entire job. When using the PRRTE runtime (the default for Open MPI), you must
use the ``--enable-recovery`` option with ``mpirun(1)`` or ``mpiexec(1)`` to
prevent the runtime from automatically aborting other processes in the job
when the local process aborts.

For example::

   mpirun --enable-recovery -n 4 ./my_program


ERRORS
------

.. include:: ./ERRORS.rst
