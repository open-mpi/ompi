.. _mpi_group_free:

MPI_Group_free
==============

.. include_body

:ref:`MPI_Group_free` |mdash| Frees a group.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_free(MPI_Group *group)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GROUP_FREE(GROUP, IERROR)
       INTEGER GROUP, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Group_free(group, ierror)
       TYPE(MPI_Group), INTENT(INOUT) :: group
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

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
