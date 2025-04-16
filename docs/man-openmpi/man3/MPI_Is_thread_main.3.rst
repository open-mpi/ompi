.. _mpi_is_thread_main:


MPI_Is_thread_main
==================

.. include_body

:ref:`MPI_Is_thread_main` |mdash| Determines if thread called :ref:`MPI_Init`

.. The following file was automatically generated
.. include:: ./bindings/mpi_is_thread_main.rst

OUTPUT PARAMETERS
-----------------
* ``flag``: True if calling thread is main thread (boolean).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Is_thread_main` is called by a thread to find out whether the caller
is the main thread (that is, the thread that called :ref:`MPI_Init` or
MPI_Init_thread).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Init_thread`
