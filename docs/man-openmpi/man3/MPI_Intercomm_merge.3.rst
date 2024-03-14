.. _mpi_intercomm_merge:


MPI_Intercomm_merge
===================

.. include_body

:ref:`MPI_Intercomm_merge` |mdash| Creates an intracommunicator from an
intercommunicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Intercomm_merge(MPI_Comm intercomm, int high,
        MPI_Comm *newintracomm)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INTERCOMM_MERGE(INTERCOMM, HIGH, NEWINTRACOMM, IERROR)
   	INTEGER	INTERCOMM, NEWINTRACOMM, IERROR
   	LOGICAL	HIGH


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Intercomm_merge(intercomm, high, newintracomm, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: intercomm
   	LOGICAL, INTENT(IN) :: high
   	TYPE(MPI_Comm), INTENT(OUT) :: newintracomm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``intercomm``: Intercommunicator (type indicator).
* ``high``: Used to order the groups of the two intracommunicators within comm when creating the new communicator (type indicator).

OUTPUT PARAMETERS
-----------------
* ``newintracomm``: Created intracommunicator (type indicator).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This function creates an intracommunicator from the union of the two
groups that are associated with intercomm. All processes should provide
the same high value within each of the two groups. If processes in one
group provide the value high = false and processes in the other group
provide the value high = true, then the union orders the "low" group
before the "high" group. If all processes provide the same high
argument, then the order of the union is arbitrary. This call is
blocking and collective within the union of the two groups.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Intercomm_create`
   * :ref:`MPI_Comm_free`
