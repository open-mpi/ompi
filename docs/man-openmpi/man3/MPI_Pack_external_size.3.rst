.. _mpi_pack_external_size:


MPI_Pack_external_size
======================

.. include_body

:ref:`MPI_Pack_external_size` |mdash| Calculates upper bound on space needed to
write to a portable format


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Pack_external_size(const char *datarep, int incount,
   	MPI_Datatype datatype, MPI_Aint *size)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_PACK_EXTERNAL_SIZE(DATAREP, INCOUNT, DATATYPE, SIZE, IERROR)

   	INTEGER		INCOUNT, DATATYPE, IERROR
   	INTEGER(KIND=MPI_ADDRESS_KIND) SIZE
   	CHARACTER*(*)	DATAREP


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Pack_external_size(datarep, incount, datatype, size, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(IN) :: incount
   	CHARACTER(LEN=*), INTENT(IN) :: datarep
   	INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: size
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``datarep``: Data representation (string).
* ``incount``: Number of input data items (integer).
* ``datatype``: Datatype of each input data item (handle).

OUTPUT PARAMETERS
-----------------
* ``size``: Upper bound on size of packed message, in bytes (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Pack_external_size` allows the application to find out how much space
is needed to pack a message in the portable format defined by the MPI
Forum. It returns in *size* an upper bound on the increment in
*position* that would occur in a call to :ref:`MPI_Pack_external` with the same
values for *datarep*, *incount*, and *datatype*.

The call returns an upper bound, rather than an exact bound, as the
exact amount of space needed to pack the message may depend on context
and alignment (e.g., the first message packed in a packing unit may take
more space).


NOTES
-----

The *datarep* argument specifies the data format. The only valid value
in the current version of MPI is "external32". The argument is provided
for future extensibility.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Pack_external`
   * :ref:`MPI_Unpack_external`
