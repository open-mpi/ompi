.. _mpi_keyval_free:


MPI_Keyval_free
===============

.. include_body

:ref:`MPI_Keyval_free` |mdash| Frees attribute key for communicator cache attribute |mdash| |deprecated_favor| :ref:`MPI_Comm_free_keyval`.

.. The following file was automatically generated
.. include:: ./bindings/mpi_keyval_free.rst

INPUT PARAMETER
---------------
* ``keyval``: Frees the integer key value (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Comm_free_keyval` instead.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Keyval_create`
   * :ref:`MPI_Comm_create_keyval`
   * :ref:`MPI_Comm_free_keyval`
