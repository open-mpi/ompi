.. _mpi_errhandler_free:


MPI_Errhandler_free
===================

.. include_body

:ref:`MPI_Errhandler_free` |mdash| Frees an MPI-style error handler.

.. The following file was automatically generated
.. include:: ./bindings/mpi_errhandler_free.rst

INPUT PARAMETER
---------------
* ``errhandler``: MPI error handler (handle). Set to MPI_ERRHANDLER_NULL on exit.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Marks the error handler associated with errhandler for deallocation and
sets errhandler to MPI_ERRHANDLER_NULL. The error handler will be
deallocated after all communicators associated with it have been
deallocated.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_errhandler`
   * :ref:`MPI_Comm_get_errhandler`
   * :ref:`MPI_Comm_set_errhandler`
