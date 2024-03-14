.. _mpi_type_get_envelope:


MPI_Type_get_envelope
=====================

.. include_body

:ref:`MPI_Type_get_envelope` |mdash| Returns information about input arguments
associated with a data type.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Type_get_envelope(MPI_Datatype datatype, int *num_integers,
   	int *num_addresses, int *num_datatypes, int *combiner)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_TYPE_GET_ENVELOPE(DATATYPE, NUM_INTEGERS, NUM_ADDRESSES,
   		NUM_DATATYPES, COMBINER, IERROR)
   	INTEGER	DATATYPE, NUM_INTEGERS, NUM_ADDRESSES
   	INTEGER	NUM_DATATYPES, COMBINER, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Type_get_envelope(datatype, num_integers, num_addresses, num_datatypes,
   		combiner, ierror)
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	INTEGER, INTENT(OUT) :: num_integers, num_addresses, num_datatypes,
   	combiner
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETER
---------------
* ``datatype``: Data type to access (handle).

OUTPUT PARAMETERS
-----------------
* ``num_integers``: Number of input integers used in the call constructing *combiner* (nonnegative integer).
* ``num_addresses``: Number of input addresses used in the call constructing *combiner* (nonnegative integer).
* ``num_datatypes``: Number of input data types used in the call constructing *combiner* (nonnegative integer).
* ``combiner``: Combiner (state).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

For the given data type, :ref:`MPI_Type_get_envelope` returns information on
the number and type of input arguments used in the call that created the
data type. The number-of-arguments values returned can be used to
provide sufficiently large arrays in the decoding routine
:ref:`MPI_Type_get_contents`. This call and the meaning of the returned values
is described below. The combiner reflects the MPI data type constructor
call that was used in creating *datatype*.


NOTES
-----

These are the values that can be returned in *combiner* and their
associated calls:

::

   Values                          Associated Calls

   MPI_COMBINER_NAMED              a named predefined data type
   MPI_COMBINER_DUP                MPI_Type_dup
   MPI_COMBINER_CONTIGUOUS         MPI_Type_contiguous
   MPI_COMBINER_VECTOR             MPI_Type_vector
   MPI_COMBINER_HVECTOR            MPI_Type_hvector
                                     and MPI_Type_create_hvector
   MPI_COMBINER_INDEXED            MPI_Type_indexed
   MPI_COMBINER_HINDEXED           MPI_Type_hindexed
                                     and MPI_Type_create_hindexed
   MPI_COMBINER_INDEXED_BLOCK      MPI_Type_create_indexed_block
   MPI_COMBINER_STRUCT             MPI_Type_struct
                                     and MPI_Type_create_struct
   MPI_COMBINER_SUBARRAY           MPI_Type_create_subarray
   MPI_COMBINER_DARRAY             MPI_Type_create_darray
   MPI_COMBINER_F90_REAL           MPI_Type_create_f90_real
   MPI_COMBINER_F90_COMPLEX        MPI_Type_create_f90_complex
   MPI_COMBINER_F90_INTEGER        MPI_Type_create_f90_integer
   MPI_COMBINER_RESIZED            MPI_Type_create_resized

If *combiner* is MPI_COMBINER_NAMED, then *datatype* is a named
predefined data type.

The actual arguments used in the creation call for a data type can be
obtained from the call :ref:`MPI_Type_get_contents`.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Type_get_contents`
