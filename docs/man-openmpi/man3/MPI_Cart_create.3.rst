.. _mpi_cart_create:

MPI_Cart_create
===============

.. include_body

:ref:`MPI_Cart_create` |mdash| Makes a new communicator to which Cartesian
topology information has been attached.

.. The following file was automatically generated
.. include:: ./bindings/mpi_cart_create.rst

INPUT PARAMETERS
----------------

* ``comm_old`` : Input communicator (handle).
* ``ndims`` : Number of dimensions of Cartesian grid (integer).
* ``dims`` : Integer array of size ndims specifying the number of
   processes in each dimension.
* ``periods`` : Logical array of size ndims specifying whether the grid
   is periodic (true) or not (false) in each dimension.
* ``reorder`` : Ranking may be reordered (true) or not (false)
   (logical).

OUTPUT PARAMETERS
-----------------

* ``comm_cart`` : Communicator with new Cartesian topology (handle).
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Cart_create` returns a handle to a new communicator to which the
Cartesian topology information is attached. If ``reorder`` = false then
the rank of each process in the new group is identical to its rank in
the old group. Otherwise, the function may ``reorder`` the processes
(possibly so as to choose a good embedding of the virtual topology onto
the physical machine). If the total size of the Cartesian grid is
smaller than the size of the group of comm, then some processes are
returned MPI_COMM_NULL, in analogy to :ref:`MPI_Comm_split`. The call
is erroneous if it specifies a grid that is larger than the group size.

ERRORS
------

.. include:: ./ERRORS.rst
