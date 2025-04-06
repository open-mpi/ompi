.. _mpi_topo_test:


MPI_Topo_test
=============

.. include_body

:ref:`MPI_Topo_test` |mdash| Determines the type of topology (if any) associated
with a communicator.

.. The following file was automatically generated
.. include:: ./bindings/mpi_topo_test.rst

INPUT PARAMETER
---------------
* ``comm``: Communicator (handle).

OUTPUT PARAMETERS
-----------------
* ``top_type``: Topology type of communicator comm (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The function :ref:`MPI_Topo_test` returns the type of topology that is assigned
to a communicator.

The output value *top_type* is one of the following:

::

       MPI_GRAPH        graph topology
       MPI_CART	        Cartesian topology
       MPI_DIST_GRAPH   distributed graph topology
       MPI_UNDEFINED    no topology


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Graph_create`
   * :ref:`MPI_Cart_create`
