.. _mpi_win_delete_attr:


MPI_Win_delete_attr
===================

.. include_body

:ref:`MPI_Win_delete_attr` |mdash| Deletes an attribute from a window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_delete_attr(MPI_Win win, int win_keyval)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_DELETE_ATTR(WIN, WIN_KEYVAL, IERROR)
   	INTEGER WIN, WIN_KEYVAL, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_delete_attr(win, win_keyval, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	INTEGER, INTENT(IN) :: win_keyval
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT/OUTPUT PARAMETER
----------------------
* ``win``: Window from which the attribute is deleted (handle).

INPUT PARAMETER
---------------
* ``win_keyval``: Key value (integer).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

NOTES
-----

Note that it is not defined by the MPI standard what happens if the
delete_fn callback invokes other MPI functions. In Open MPI, it is not
valid for delete_fn callbacks (or any of their children) to add or
delete attributes on the same object on which the delete_fn callback is
being invoked.


ERRORS
------

.. include:: ./ERRORS.rst
