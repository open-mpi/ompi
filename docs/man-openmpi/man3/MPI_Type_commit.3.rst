.. _mpi_type_commit:


MPI_Type_commit
===============

.. include_body

:ref:`MPI_Type_commit` |mdash| Commits a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_commit(MPI_Datatype *datatype)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_COMMIT(DATATYPE, IERROR)
   	INTEGER	DATATYPE, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_commit(datatype, ierror)
   	TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``datatype``: Data type (handle).

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The commit operation commits the data type. A data type is the formal
description of a communication buffer, not the content of that buffer.
After a data type has been committed, it can be repeatedly reused to
communicate the changing content of a buffer or, indeed, the content of
different buffers, with different starting addresses.

**Example:** The following Fortran code fragment gives examples of using
:ref:`MPI_Type_commit`.

.. code-block:: fortran

       INTEGER :: type1, type2
       CALL MPI_TYPE_CONTIGUOUS(5, MPI_REAL, type1, ierr)
                     ! new type object created
       CALL MPI_TYPE_COMMIT(type1, ierr)
                     ! now type1 can be used for communication

If the data type specified in *datatype* is already committed, it is
equivalent to a no-op.


ERRORS
------

.. include:: ./ERRORS.rst
