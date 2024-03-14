.. _mpi_cart_sub:

MPI_Cart_sub
============

.. include_body

:ref:`MPI_Cart_sub` |mdash| Partitions a communicator into subgroups, which form
lower-dimensional Cartesian subgrids.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Cart_sub(MPI_Comm comm, const int remain_dims[], MPI_Comm *comm_new)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CART_SUB(COMM, REMAIN_DIMS, COMM_NEW, IERROR)
       INTEGER COMM, COMM_NEW, IERROR
       LOGICAL REMAIN_DIMS(*)

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Cart_sub(comm, remain_dims, newcomm, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       LOGICAL, INTENT(IN) :: remain_dims(*)
       TYPE(MPI_Comm), INTENT(OUT) :: newcomm
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``comm`` : Communicator with Cartesian structure (handle).
* ``remain_dims`` : The ith entry of remain_dims specifies whether the ith
   dimension is kept in the subgrid (true) or is dropped (false)
   (logical vector).

OUTPUT PARAMETERS
-----------------

* ``comm_new`` : Communicator containing the subgrid that includes the
   calling process (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

If a Cartesian topology has been created with :ref:`MPI_Cart_create`, the
function :ref:`MPI_Cart_sub` can be used to partition the communicator group
into subgroups that form lower-dimensional Cartesian subgrids, and to
build for each subgroup a communicator with the associated subgrid
Cartesian topology. (This function is closely related to
:ref:`MPI_Comm_split`.)

Example: Assume that ``MPI_Cart_create( ..., comm)`` has defined a (2 x 3 x
4) grid. Let remain_dims = (true, false, true). Then a call to

.. code-block:: c

   MPI_Cart_sub(comm, remain_dims, comm_new);

will create three communicators, each with eight processes in a 2 x 4
Cartesian topology. If remain_dims = (false, false, true) then the call
to MPI_Cart_sub(comm, remain_dims, comm_new) will create six
nonoverlapping communicators, each with four processes, in a
one-dimensional Cartesian topology.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Cart_create`
