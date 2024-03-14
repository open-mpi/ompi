.. _mpi_comm_set_info:


MPI_Comm_set_info
=================

.. include_body

:ref:`MPI_Comm_set_info` |mdash| Set communicator info hints


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_set_info(MPI_Comm comm, MPI_Info info)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_SET_INFO(COMM, INFO, IERROR)
   	INTEGER	COMM, INFO, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_set_info(comm, info, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(IN) :: info
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``comm``: Communicator on which to set info hints
* ``info``: Info object containing hints to be set on *comm*

OUTPUT PARAMETERS
-----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_set_info` sets new values for the hints of the communicator
associated with *comm*. :ref:`MPI_Comm_set_info` is a collective routine. The
info object may be different on each process, but any info entries that
an implementation requires to be the same on all processes must appear
with the same value in each process's *info* object.

The following info key assertions may be accepted by Open MPI:

*mpi_assert_no_any_tag* (boolean): If set to true, then the
implementation may assume that the process will not use the ``MPI_ANY_TAG``
wildcard on the given communicator.

*mpi_assert_no_any_source* (boolean): If set to true, then the
implementation may assume that the process will not use the
``MPI_ANY_SOURCE`` wildcard on the given communicator.

*mpi_assert_exact_length* (boolean): If set to true, then the
implementation may assume that the lengths of messages received by the
process are equal to the lengths of the corresponding receive buffers,
for point-to-point communication operations on the given communicator.

*mpi_assert_allow_overtaking* (boolean): If set to true, then the
implementation may assume that point-to-point communications on the
given communicator do not rely on the non-overtaking rule specified in
MPI-3.1 Section 3.5. In other words, the application asserts that send
operations are not required to be matched at the receiver in the order
in which the send operations were performed by the sender, and receive
operations are not required to be matched in the order in which they
were performed by the receiver.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_get_info`
   * :ref:`MPI_Info_create`
   * :ref:`MPI_Info_set`
   * :ref:`MPI_Info_free`
