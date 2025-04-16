.. _mpi_win_delete_attr:


MPI_Win_delete_attr
===================

.. include_body

:ref:`MPI_Win_delete_attr` |mdash| Deletes an attribute from a window.

.. The following file was automatically generated
.. include:: ./bindings/mpi_win_delete_attr.rst

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
