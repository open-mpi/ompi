.. _mpi_buffer_attach:

MPI_Buffer_attach
=================

.. include_body

:ref:`MPI_Buffer_attach` |mdash| Attaches a user-defined buffer for sending.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: C

   #include <mpi.h>

   int MPI_Buffer_attach(void *buf, int size)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_BUFFER_ATTACH(BUF, SIZE, IERROR)
       <type>  BUF(*)
       INTEGER SIZE, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: Fortran

   USE mpi_f08

   MPI_Buffer_attach(buffer, size, ierror)
       TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buffer
       INTEGER, INTENT(IN) :: size
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

INPUT PARAMETERS
----------------

* ``buf`` : Initial buffer address (choice).
* ``size`` : Buffer size, in bytes (integer).

OUTPUT PARAMETER
----------------

* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

Provides to MPI a buffer in the user's memory to be used for buffering
outgoing messages. The buffer is used only by messages sent in buffered
mode. Only one buffer can be attached to a process at a time.

NOTES
-----

The size given should be the sum of the sizes of all outstanding Bsends
that you intend to have, plus MPI_BSEND_OVERHEAD bytes for each Bsend
that you do. For the purposes of calculating size, you should use
:ref:`MPI_Pack_size`. In other words, in the code

.. code-block:: c

   MPI_Buffer_attach( buf, size )
   MPI_Bsend( ..., count=20, datatype=type1, ... );
   MPI_Bsend( ..., count=40, datatype=type2, ... );

the value of size in the :ref:`MPI_Buffer_attach` call should be greater than
the value computed by

.. code-block:: c

   MPI_Pack_size( 20, type1, comm, &s1 );
   MPI_Pack_size( 40, type2, comm, &s2 );
   size = s1 + s2 + 2 * MPI_BSEND_OVERHEAD;

MPI_BSEND_OVERHEAD gives the maximum amount of buffer space that may be
used by the Bsend routines. This value is in mpi.h for C and mpif.h for
Fortran.

ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Buffer_detach`
