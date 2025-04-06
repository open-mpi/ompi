.. _mpi_type_create_indexed_block:


MPI_Type_create_indexed_block
=============================

.. include_body

:ref:`MPI_Type_create_indexed_block`, :ref:`MPI_Type_create_hindexed_block` -
Creates an indexed data type with the same block length for all blocks.

.. The following directive tells the man page generation script to
   generate multiple bindings for this file.
.. mpi-bindings: MPI_Type_create_indexed_block, MPI_Type_create_hindexed_block

.. The following file was automatically generated
.. include:: ./bindings/mpi_type_create_indexed_block.rst

INPUT PARAMETERS
----------------
* ``count``: Length of array of displacements (integer).
* ``blocklength``: Size of block (integer).
* ``array_of_displacements``: Array of displacements (array of integers). In units of the extent of *oldtype* for MPI_Type_create_indexed_block and bytes for MPI_Type_create_hindexed_block.
* ``oldtype``: Old data type (handle).

OUTPUT PARAMETERS
-----------------
* ``newtype``: New data type (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Type_create_indexed_block` and :ref:`MPI_Type_create_hindexed_block` create
an indexed data type with the same block length for all blocks. The only
difference between the two functions is :ref:`MPI_Type_create_indexed_block`
takes an array of displacements in units of the extent of *oldtype*
while :ref:`MPI_Type_create_hindexed_block` takes displacements in bytes.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_indexed`
