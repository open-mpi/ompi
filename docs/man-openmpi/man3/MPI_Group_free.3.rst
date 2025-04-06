.. _mpi_group_free:

MPI_Group_free
==============

.. include_body

:ref:`MPI_Group_free` |mdash| Frees a group.

.. The following file was automatically generated
.. include:: ./bindings/mpi_group_free.rst

INPUT/OUTPUT PARAMETER
----------------------

* ``group`` : Group (handle).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This operation marks a ``group`` object for deallocation. The handle
``group`` is set to MPI_GROUP_NULL by the call. Any ongoing
operation using this ``group`` will complete normally.

NOTE
----

On return, ``group`` is set to MPI_GROUP_NULL.

ERRORS
------

.. include:: ./ERRORS.rst
