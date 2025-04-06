.. _mpi_win_get_info:


MPI_Win_get_info
================

.. include_body

:ref:`MPI_Win_get_info` |mdash| Retrieves active window info hints

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_get_info.rst

INPUT PARAMETERS
----------------
* ``win``: Window from which to receive active info hints

OUTPUT PARAMETERS
-----------------
* ``info_used``: New info object returned with all active hints on this window.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Win_get_info` returns a new info object containing the hints of the
window associated with *win*. The current setting of all hints actually
used by the system related to this window is returned in *info_used*. If
no such hints exist, a handle to a newly created info object is returned
that contains no key/value pair. The user is responsible for freeing
info_used via :ref:`MPI_Info_free`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Win_set_info`
   * :ref:`MPI_Win_free`
