.. _mpi_group_size:


MPI_Group_size
==============

.. include_body

:ref:`MPI_Group_size` |mdash| Returns the size of a group.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Group_size(MPI_Group group, int *size)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_GROUP_SIZE(GROUP, SIZE, IERROR)
   	INTEGER	GROUP, SIZE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Group_size(group, size, ierror)
   	TYPE(MPI_Group), INTENT(IN) :: group
   	INTEGER, INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


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
