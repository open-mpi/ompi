.. _mpi_cart_shift:

MPI_Cart_shift
==============

.. include_body

:ref:`MPI_Cart_shift` |mdash| Returns the shifted source and destination ranks,
given a shift direction and amount.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Cart_shift(MPI_Comm comm, int direction, int disp,
       int *rank_source, int *rank_dest)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_CART_SHIFT(COMM, DIRECTION, DISP, RANK_SOURCE,
           RANK_DEST, IERROR)
       INTEGER COMM, DIRECTION, DISP, RANK_SOURCE
       INTEGER RANK_DEST, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest, ierror)
       TYPE(MPI_Comm), INTENT(IN) :: comm
       INTEGER, INTENT(IN) :: direction, disp
       INTEGER, INTENT(OUT) :: rank_source, rank_dest
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``comm`` : Communicator with Cartesian structure (handle).
* ``direction`` : Coordinate dimension of shift (integer).
* ``disp`` : Displacement ( > 0: upward shift, < 0: downward shift)
   (integer).

OUTPUT PARAMETERS
-----------------

* ``rank_source`` : Rank of source process (integer).
* ``rank_dest`` : Rank of destination process (integer).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

If the process topology is a Cartesian structure, an :ref:`MPI_Sendrecv`
operation is likely to be used along a coordinate ``direction`` to
perform a shift of data. As input, :ref:`MPI_Sendrecv` takes the rank of a
source process for the receive, and the rank of a destination process
for the send. If the function :ref:`MPI_Cart_shift` is called for a
Cartesian process group, it provides the calling process with the above
identifiers, which then can be passed to :ref:`MPI_Sendrecv`. The user
specifies the coordinate ``direction`` and the size of the step
(positive or negative). The function is local.

The ``direction`` argument indicates the dimension of the shift, i.e.,
the coordinate whose value is modified by the shift. The coordinates are
numbered from 0 to ndims-1, where ndims is the number of dimensions.

Note: The ``direction`` argument is in the range [0, n-1] for an
n-dimensional Cartesian mesh.

Depending on the periodicity of the Cartesian group in the specified
coordinate ``direction``, :ref:`MPI_Cart_shift` provides the identifiers
for a circular or an end-off shift. In the case of an end-off shift, the
value ``MPI_PROC_NULL`` may be returned in ``rank_source`` or
``rank_dest``, indicating that the source or the destination for the
shift is out of range.

Example: The ``comm``\ unicator, ``comm``, has a two-dimensional,
periodic, Cartesian topology associated with it. A two-dimensional array
of REALs is stored one element per process, in variable A. One wishes to
skew this array, by shifting column i (vertically, i.e., along the
column) by i steps.

.. code-block:: fortran

   ! find process rank
       CALL MPI_COMM_RANK(comm, rank, ierr)
   ! find Cartesian coordinates
       CALL MPI_CART_COORDS(comm, rank, maxdims, coords, ierr)
   ! compute shift source and destination
       CALL MPI_CART_SHIFT(comm, 0, coords(2), source, dest, ierr)
   ! skew array
       CALL MPI_SENDRECV_REPLACE(A, 1, MPI_REAL, dest, 0, source, 0, comm, status,
                                 ierr)

NOTE
----

In Fortran, the dimension indicated by DIRECTION = i has DIMS(i+1)
nodes, where DIMS is the array that was used to create the grid. In C,
the dimension indicated by direction = i is the dimension specified by
dims[i].

ERRORS
------

.. include:: ./ERRORS.rst
