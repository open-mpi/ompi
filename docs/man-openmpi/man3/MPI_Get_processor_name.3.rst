.. _mpi_get_processor_name:

MPI_Get_processor_name
======================

.. include_body

:ref:`MPI_Get_processor_name` |mdash| Gets the name of the processor.

SYNTAX
------

C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Get_processor_name(char *name, int *resultlen)

Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_GET_PROCESSOR_NAME(NAME, RESULTLEN, IERROR)
       CHARACTER*(*)   NAME
       INTEGER     RESULTLEN, IERROR

Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08

   MPI_Get_processor_name(name, resultlen, ierror)
       CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME), INTENT(OUT) :: name
       INTEGER, INTENT(OUT) :: resultlen
       INTEGER, OPTIONAL, INTENT(OUT) :: ierror

OUTPUT PARAMETERS
-----------------

* ``name`` : A unique specifier for the actual (as opposed to virtual)
  node.
* ``resultlen`` : Length (in characters) of result returned in name.
* ``ierror`` : Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine returns the ``name`` of the processor on which it was
called at the moment of the call. The ``name`` is a character string for
maximum flexibility. From this value it must be possible to identify a
specific piece of hardware. The argument ``name`` must represent storage
that is at least MPI_MAX_PROCESSOR_NAME characters long.

The number of characters actually written is returned in the output
argument, ``resultlen``.

NOTES
-----

The user must provide at least MPI_MAX_PROCESSOR_NAME space to write
the processor ``name``; processor ``name``\ s can be this long. The user
should examine the output argument, ``resultlen``, to determine the
actual length of the ``name``.

ERRORS
------

.. include:: ./ERRORS.rst
