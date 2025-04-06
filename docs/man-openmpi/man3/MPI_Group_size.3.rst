.. _mpi_group_size:


MPI_Group_size
==============

.. include_body

:ref:`MPI_Group_size` |mdash| Returns the size of a group.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_size.rst

INPUT PARAMETERS
----------------
* ``group``: Group (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Number of processes in the group (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Group_size` returns in *size* the number of processes in the group.
Thus, if group = MPI_GROUP_EMPTY, then the call will return size = 0. On
the other hand, a call with group = MPI_GROUP_NULL is erroneous.


ERRORS
------

.. include:: ./ERRORS.rst
