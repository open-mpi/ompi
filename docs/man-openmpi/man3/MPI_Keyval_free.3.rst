.. _mpi_keyval_free:


MPI_Keyval_free
===============

.. include_body

:ref:`MPI_Keyval_free` |mdash| Frees attribute key for communicator cache attribute -- |deprecated_favor| :ref:`MPI_Comm_free_keyval`.

SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Keyval_free(int *keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_KEYVAL_FREE(KEYVAL, IERROR)
   	INTEGER	KEYVAL, IERROR


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
