.. _mpi_win_attach:


MPI_Win_attach
==============

.. include_body

:ref:`MPI_Win_attach`, :ref:`MPI_Win_detach` - One-sided MPI call that attaches /
detaches a memory region to / from a window object for RMA operations.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Win_attach, MPI_Win_detach

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_attach.rst

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
