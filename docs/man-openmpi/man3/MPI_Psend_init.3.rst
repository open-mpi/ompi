.. _mpi_psend_init:


MPI_Psend_init
==============

.. include_body

:ref:`MPI_Psend_init` |mdash| Initializes a partitioned send.

.. The following file was automatically generated
.. include:: ./bindings/mpi_psend_init.rst

INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``partitions``: Number of partitions (integer).
* ``count``: Number of elements to be sent per partition (integer).
* ``datatype``: Datatype of each element (handle).
* ``dest``: Rank of source (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).
* ``info``: Info argument (handle).

OUTPUT PARAMETERS
-----------------
* ``request``: Communication request (handle).
* ``ierror``: Fortran only: Error status (integer).

ERRORS
------

.. include:: ./ERRORS.rst

NOTE
----

The current implementation is an early prototype and is not fully
compliant with the MPI-4.0 specification. Specifically this function and
it's counterpart (MPI_Precv_init) will block until the partitioned
communication request is initialized on both ends. This behavior will be
corrected in future versions.


.. seealso::
   * :ref:`MPI_Precv_init`
