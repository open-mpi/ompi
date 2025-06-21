.. _mpi_initialized:


MPI_Initialized
===============

.. include_body

:ref:`MPI_Initialized` |mdash| Checks whether the MPI world model has been initialized

.. The following file was automatically generated
.. include:: ./bindings/mpi_initialized.rst

OUTPUT PARAMETERS
-----------------
* ``flag``: True if the MPI world model has been initialized, and false otherwise (logical).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine may be used to determine whether the MPI world model has
been initialized.  A different routine |mdash| :ref:`MPI_Finalized`
|mdash| is used to indicate whether the MPI world model has been
finalized.

See `MPI-5.0:11.4.1 <https://www.mpi-forum.org/>`_ for a list of MPI
functionality that is available (e.g., even when the MPI
world model has not yet initialized or has already been finalized).


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Init_thread`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
   * :ref:`MPI_Session_init`
   * :ref:`MPI_Session_finalize`
