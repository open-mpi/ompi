.. _mpi_win_set_info:


MPI_Win_set_info
================

.. include_body

:ref:`MPI_Win_set_info` |mdash| Set window info hints


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Win_set_info(MPI_Win win, MPI_Info info)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_SET_INFO(WIN, INFO, IERROR)
   	INTEGER	WIN, INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_set_info(win, info, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: Window on which to set info hints
* ``info``: Info object containing hints to be set on *win*

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_WIN_SET_INFO` sets new values for the hints of the window associated
with *win.* :ref:`MPI_WIN_SET_INFO` is a collective routine. The info object
may be different on each process, but any info entries that an
implementation requires to be the same on all processes must appear with
the same value in each process's *info* object.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_get_info`
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_set`
   * :ref:`MPI_Info_free`
