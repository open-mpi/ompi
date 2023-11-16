.. _mpi_comm_get_info:


MPI_Comm_get_info
=================

.. include_body

:ref:`MPI_Comm_get_info` |mdash| Retrieves active communicator info hints


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Comm_get_info(MPI_Comm comm, MPI_Info *info_used)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_COMM_GET_INFO(COMM, INFO_USED, IERROR)
   	INTEGER	COMM, INFO_USED, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Comm_get_info(comm, info_used, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(OUT) :: info_used
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``comm``: Communicator from which to receive active info hints

OUTPUT PARAMETERS
-----------------
* ``info_used``: New info object returned with all active hints on this communicator.
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Comm_get_info` returns a new info object containing the hints of the
communicator associated with *comm*. The current setting of all hints
actually used by the system related to this communicator is returned in
*info_used*. If no such hints exist, a handle to a newly created info
object is returned that contains no key/value pair. The user is
responsible for freeing info_used via :ref:`MPI_Info_free`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_get_info`
   * :ref:`MPI_Info_free`
