.. _mpi_group_rank:


MPI_Group_rank
==============

.. include_body

:ref:`MPI_Group_rank` |mdash| Returns the rank of the calling process in the
given group.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_rank.rst

INPUT PARAMETERS
----------------
* ``group``: Group (handle).

OUTPUT PARAMETERS
-----------------
* ``rank``: Rank of the calling process in group, or MPI_UNDEFINED if the process is not a member (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Group_rank` returns as the output parameter *rank* the rank of the
calling process in group. If the process is not a member of group then
MPI_UNDEFINED is returned.


ERRORS
------

.. include:: ./ERRORS.rst
