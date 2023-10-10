.. _mpi_psend_init:


PMIX_Psend_init
==============

.. include_body

:ref:`PMIX_Psend_init` - Initializes a partitioned send.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int PMIX_Psend_init(const void *buf, int partitions, MPI_Count count, MPI_Datatype datatype, int dest,
   	int tag, MPI_Comm comm, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPIX_PSEND_INIT(BUF, PARTITIONS, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)
   	<type>	BUF(*)
   	INTEGER	PARTITIONS, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR
        INTEGER(KIND=MPI_COUNT_KIND) COUNT


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   PMIX_Psend_init(buf, partitions, count, datatype, dest, tag, comm, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
   	INTEGER, INTENT(IN) :: partitions, dest, tag
   	INTEGER(KIND=MPI_COUNT_KIND), INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``buf``: Initial address of send buffer (choice).
* ``partitions``: Number of partitions (integer).
* ``count``: Number of elements to be sent per partition (MPI_Count).
* ``datatype``: Datatype of each element (handle).
* ``dest``: Rank of source (integer).
* ``tag``: Message tag (integer).
* ``comm``: Communicator (handle).

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

The MPIX prefix is used to denote that this function's API will likely change
in the MPI-4.1 specification.

.. seealso::
   * :ref:`MPIX_Precv_init`
