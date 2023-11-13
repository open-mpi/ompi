.. _mpi_win_get_group:


MPI_Win_get_group
=================

.. include_body

:ref:`MPI_Win_get_group` |mdash| Returns a duplicate of the group of the
communicator used to create the window.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   MPI_Win_get_group(MPI_Win win, MPI_Group *group)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_GET_GROUP(WIN, GROUP, IERROR)
   	INTEGER WIN, GROUP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_get_group(win, group, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(MPI_Group), INTENT(OUT) :: group
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``win``: Window object (handle).

OUTPUT PARAMETERS
-----------------
* ``group``: Group of processes that share access to the window (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_get_group` returns a duplicate of the group of the communicator
used to create the window associated with *win*. The group is returned
in *group*.


ERRORS
------

.. include:: ./ERRORS.rst
