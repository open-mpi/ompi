.. _mpi_win_attach:


MPI_Win_attach
==============

.. include_body

:ref:`MPI_Win_attach`, :ref:`MPI_Win_detach` - One-sided MPI call that attaches /
detaches a memory region to / from a window object for RMA operations.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   MPI_Win_attach(MPI_Win win, void *base, MPI_Aint size)

   MPI_Win_detach(MPI_Win win, void *base)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_WIN_ATTACH(WIN, BASE, SIZE, IERROR)
   	<type> BASE(*)
   	INTEGER(KIND=MPI_ADDRESS_KIND) SIZE
   	INTEGER WIN, IERROR

   MPI_WIN_DETACH(WIN, BASE, IERROR)
   	<type> BASE(*)
   	INTEGER WIN, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Win_attach(win, base, size, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(*), DIMENSION(..), INTENT(IN) :: base
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Win_detach(win, base, ierror)
   	TYPE(MPI_Win), INTENT(IN) :: win
   	TYPE(*), DIMENSION(..), INTENT(IN) :: base
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``win``: A window that was created with *MPI_Win_create_dynamic*
* ``base``: Initial address of window (choice).
* ``size``: Size of window in bytes (nonnegative integer).

OUTPUT PARAMETERS
-----------------
* ``win``: Window object returned by the call (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_attach` is a one-sided MPI communication call used to attach a
memory region of *size* bytes starting at address *base* to a window for
RMA access. The window *win* must have been created using
:ref:`MPI_Win_create_dynamic`. Multiple non-overlapping memory regions may be
attached to the same dynamic window. Attaching overlapping memory
regions to the same dynamic window is erroneous.

If the *base* value used by :ref:`MPI_Win_attach` was allocated by
:ref:`MPI_Alloc_mem`, the size of the window can be no larger than the value
set by the :ref:`MPI_ALLOC_MEM` function.

:ref:`MPI_Win_detach` can be used to detach a previously attached memory region
from *win*. The memory address *base* and *win* must match arguments
passed to a previous call to :ref:`MPI_Win_attach`.


NOTES
-----

Use memory allocated by :ref:`MPI_Alloc_mem` to guarantee properly aligned
window boundaries (such as word, double-word, cache line, page frame,
and so on).


ERRORS
------

.. include:: ./ERRORS.rst
