.. _mpi_comm_compare:

MPI_Comm_compare
================

.. include_body

:ref:`MPI_Comm_compare` |mdash| Compares two communicators.

.. The following file was automatically generated
.. include:: ./bindings/mpi_comm_compare.rst

INPUT PARAMETERS
----------------

* ``comm1`` : Comm1 (handle).
* ``comm2`` : Comm2 (handle).

OUTPUT PARAMETERS
-----------------

* ``result`` : Result of comparison (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

MPI_IDENT ``result``\ s if and only if ``comm1`` and ``comm2`` are
handles for the same object (identical groups and same contexts).
MPI_CONGRUENT results if the underlying groups are identical in
constituents and rank order; these communicators differ only by context.
MPI_SIMILAR results of the group members of both communicators are
the same but the rank order differs. MPI_UNEQUAL results otherwise.

ERRORS
------

.. include:: ./ERRORS.rst
