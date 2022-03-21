.. _mpi_topo_test:


MPI_Topo_test
=============

.. include_body

:ref:`MPI_Topo_test` - Determines the type of topology (if any) associated
with a communicator.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Topo_test(MPI_Comm comm, int *top_type)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TOPO_TEST(COMM, TOP_TYPE, IERROR)
   	INTEGER	COMM, TOP_TYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Topo_test(comm, status, ierror)
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, INTENT(OUT) :: status
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror
   		DOUBLE PRECISION MPI_Wtick()
   		DOUBLE PRECISION MPI_Wtime()


INPUT PARAMETER
---------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``top_type``: Topology type of communicator comm (choice).
* ``IERROR``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Topo_test` returns the type of topology that is assigned
to a communicator.

The output value *top_type* is one of the following:

::

       MPI_GRAPH		graph topology
       MPI_CART		Cartesian topology
       MPI_DIST_GRAPH	distributed graph topology
       MPI_UNDEFINED	no topology


ERRORS
------

Almost all MPI routines return an error value; C routines as the value
of the function and Fortran routines in the last argument.

Before the error value is returned, the current MPI error handler is
called. By default, this error handler aborts the MPI job, except for
I/O function errors. The error handler may be changed with
:ref:`MPI_Comm_set_errhandler`; the predefined error handler MPI_ERRORS_RETURN
may be used to cause error values to be returned. Note that MPI does not
guarantee that an MPI program can continue past an error.


.. seealso::
   :ref:`MPI_Graph_create` :ref:`MPI_Cart_create`
