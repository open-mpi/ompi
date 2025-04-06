.. _mpi_group_translate_ranks:


MPI_Group_translate_ranks
=========================

.. include_body

:ref:`MPI_Group_translate_ranks` |mdash| Translates the ranks of processes in one
group to those in another group.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_translate_ranks.rst

INPUT PARAMETERS
----------------
* ``group1``: First group (handle).
* ``n``: Number of ranks in ranks1 and ranks2 arrays (integer).
* ``ranks1``: Array of zero or more valid ranks in group1.
* ``group2``: Second group (handle).

OUTPUT PARAMETERS
-----------------
* ``ranks2``: Array of corresponding ranks in group2, MPI_UNDEFINED when no correspondence exists.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function is important for determining the relative numbering of the
same processes in two different groups. For instance, if one knows the
ranks of certain processes in the group of MPI_COMM_WORLD, one might
want to know their ranks in a subset of that group.


ERRORS
------

.. include:: ./ERRORS.rst
