.. _mpi_reduce_local:


MPI_Reduce_local
================

.. include_body

:ref:`MPI_Reduce_local` |mdash| Perform a local reduction


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Reduce_local(const void *inbuf, void *inoutbuf, int count,
   	MPI_Datatype datatype, MPI_Op op)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_REDUCE_LOCAL(INBUF, INOUTBUF, COUNT, DATATYPE, OP, IERROR)
   	<type>	INBUF(*), INOUTBUF(*)
   	INTEGER	COUNT, DATATYPE, OP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Reduce_local(inbuf, inoutbuf, count, datatype, op, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
   	TYPE(*), DIMENSION(..) :: inoutbuf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Op), INTENT(IN) :: op
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``inbuf``: Address of input buffer (choice).
* ``count``: Number of elements in input buffer (integer).
* ``datatype``: Data type of elements of input buffer (handle).
* ``op``: Reduce operation (handle).

OUTPUT PARAMETERS
-----------------
* ``inoutbuf``: Address of in/out buffer (choice).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

The global reduce functions (:ref:`MPI_Reduce_local`, :ref:`MPI_Op_create`,
:ref:`MPI_Op_free`, :ref:`MPI_Allreduce`, MPI_Reduce_local_scatter, MPI_Scan) perform
a global reduce operation (such as sum, max, logical AND, etc.) across
all the members of a group. The reduction operation can be either one of
a predefined list of operations, or a user-defined operation. The global
reduction functions come in several flavors: a reduce that returns the
result of the reduction at one node, an all-reduce that returns this
result at all nodes, and a scan (parallel prefix) operation. In
addition, a reduce-scatter operation combines the functionality of a
reduce and a scatter operation.

:ref:`MPI_Reduce_local` combines the elements provided in the input and
input/output buffers of the local process, using the operation op, and
returns the combined value in the inout/output buffer. The input buffer
is defined by the arguments inbuf, count, and datatype; the output
buffer is defined by the arguments inoutbuf, count, and datatype; both
have the same number of elements, with the same type. The routine is a
local call. The process can provide one element, or a sequence of
elements, in which case the combine operation is executed element-wise
on each entry of the sequence. For example, if the operation is MPI_MAX
and the input buffer contains two elements that are floating-point
numbers (count = 2 and datatype = MPI_FLOAT), then inoutbuf(1) = global
max (inbuf(1)) and inoutbuf(2) = global max(inbuf(2)).


USE OF IN-PLACE OPTION
----------------------

The use of MPI_IN_PLACE is disallowed with :ref:`MPI_Reduce_local`.


PREDEFINED REDUCE OPERATIONS
----------------------------

The set of predefined operations provided by MPI is listed below
(Predefined Reduce Operations). That section also enumerates the
datatypes each operation can be applied to. In addition, users may
define their own operations that can be overloaded to operate on several
datatypes, either basic or derived. This is further explained in the
description of the user-defined operations (see the man pages for
:ref:`MPI_Op_create` and MPI_Op_free).

The operation op is always assumed to be associative. All predefined
operations are also assumed to be commutative. Users may define
operations that are assumed to be associative, but not commutative. The
\``canonical'' evaluation order of a reduction is determined by the
ranks of the processes in the group. However, the implementation can
take advantage of associativity, or associativity and commutativity, in
order to change the order of evaluation. This may change the result of
the reduction for operations that are not strictly associative and
commutative, such as floating point addition.

Predefined operators work only with the MPI types listed below
(Predefined Reduce Operations, and the section MINLOC and MAXLOC,
below). User-defined operators may operate on general, derived
datatypes. In this case, each argument that the reduce operation is
applied to is one element described by such a datatype, which may
contain several basic values. This is further explained in Section 4.9.4
of the MPI Standard, "User-Defined Operations."

The following predefined operations are supplied for :ref:`MPI_Reduce_local`
and related functions :ref:`MPI_Allreduce`, :ref:`MPI_Reduce_scatter`, and :ref:`MPI_Scan`.
These operations are invoked by placing the following in op:

::

   	Name                Meaning
        ---------           --------------------
   	MPI_MAX             maximum
   	MPI_MIN             minimum
   	MPI_SUM             sum
   	MPI_PROD            product
   	MPI_LAND            logical and
   	MPI_BAND            bit-wise and
   	MPI_LOR             logical or
   	MPI_BOR             bit-wise or
   	MPI_LXOR            logical xor
   	MPI_BXOR            bit-wise xor
   	MPI_MAXLOC          max value and location
   	MPI_MINLOC          min value and location

The two operations MPI_MINLOC and MPI_MAXLOC are discussed separately
below (MINLOC and MAXLOC). For the other predefined operations, we
enumerate below the allowed combinations of op and datatype arguments.
First, define groups of MPI basic datatypes in the following way:

::

   	C integer:            MPI_INT, MPI_LONG, MPI_SHORT,
   	                      MPI_UNSIGNED_SHORT, MPI_UNSIGNED,
   	                      MPI_UNSIGNED_LONG
   	Fortran integer:      MPI_INTEGER
   	Floating-point:       MPI_FLOAT, MPI_DOUBLE, MPI_REAL,
   	                      MPI_DOUBLE_PRECISION, MPI_LONG_DOUBLE
   	Logical:              MPI_LOGICAL
   	Complex:              MPI_COMPLEX
   	Byte:                 MPI_BYTE

Now, the valid datatypes for each option is specified below.

::

   	Op                      	Allowed Types
        ----------------         ---------------------------
   	MPI_MAX, MPI_MIN		C integer, Fortran integer,
   						floating-point

   	MPI_SUM, MPI_PROD 		C integer, Fortran integer,
   						floating-point, complex

   	MPI_LAND, MPI_LOR,		C integer, logical
   	MPI_LXOR

   	MPI_BAND, MPI_BOR,		C integer, Fortran integer, byte
   	MPI_BXOR


MINLOC AND MAXLOC
-----------------

The operator MPI_MINLOC is used to compute a global minimum and also an
index attached to the minimum value. MPI_MAXLOC similarly computes a
global maximum and index. One application of these is to compute a
global minimum (maximum) and the rank of the process containing this
value.

The operation that defines MPI_MAXLOC is

::

            ( u )    (  v )      ( w )
            (   )  o (    )   =  (   )
            ( i )    (  j )      ( k )

   where

       w = max(u, v)

   and

            ( i            if u > v
            (
      k   = ( min(i, j)    if u = v
            (
            (  j           if u < v)


MPI_MINLOC is defined similarly:

::
   
            ( u )    (  v )      ( w )
            (   )  o (    )   =  (   )
            ( i )    (  j )      ( k )

   where

       w = min(u, v)

   and

            ( i            if u < v
            (
      k   = ( min(i, j)    if u = v
            (
            (  j           if u > v)

Both operations are associative and commutative. Note that if MPI_MAXLOC
is applied to reduce a sequence of pairs (u(0), 0), (u(1), 1), ...,
(u(n-1), n-1), then the value returned is (u , r), where u= max(i) u(i)
and r is the index of the first global maximum in the sequence. Thus, if
each process supplies a value and its rank within the group, then a
reduce operation with op = MPI_MAXLOC will return the maximum value and
the rank of the first process with that value. Similarly, MPI_MINLOC can
be used to return a minimum and its index. More generally, MPI_MINLOC
computes a lexicographic minimum, where elements are ordered according
to the first component of each pair, and ties are resolved according to
the second component.

The reduce operation is defined to operate on arguments that consist of
a pair: value and index. For both Fortran and C, types are provided to
describe the pair. The potentially mixed-type nature of such arguments
is a problem in Fortran. The problem is circumvented, for Fortran, by
having the MPI-provided type consist of a pair of the same type as
value, and coercing the index to this type also. In C, the MPI-provided
pair type has distinct types and the index is an int.

In order to use MPI_MINLOC and MPI_MAXLOC in a reduce operation, one
must provide a datatype argument that represents a pair (value and
index). MPI provides nine such predefined datatypes. The operations
MPI_MAXLOC and MPI_MINLOC can be used with each of the following
datatypes:

::

       Fortran:
       Name                     Description
       MPI_2REAL                pair of REALs
       MPI_2DOUBLE_PRECISION    pair of DOUBLE-PRECISION variables
       MPI_2INTEGER             pair of INTEGERs

       C:
       Name        	    	Description
       MPI_FLOAT_INT            float and int
       MPI_DOUBLE_INT           double and int
       MPI_LONG_INT             long and int
       MPI_2INT                 pair of ints
       MPI_SHORT_INT            short and int
       MPI_LONG_DOUBLE_INT      long double and int

The data type ``MPI_2REAL`` is equivalent to:

.. code-block:: fortran

       call MPI_TYPE_CONTIGUOUS(2, MPI_REAL, MPI_2REAL)

Similar statements apply for MPI_2INTEGER, MPI_2DOUBLE_PRECISION, and
MPI_2INT.

The datatype MPI_FLOAT_INT is as if defined by the following sequence of
instructions.

::.. code-block:: c

       type[0] = MPI_FLOAT
       type[1] = MPI_INT
       disp[0] = 0
       disp[1] = sizeof(float)
       block[0] = 1
       block[1] = 1
       MPI_TYPE_STRUCT(2, block, disp, type, MPI_FLOAT_INT)

Similar statements apply for MPI_LONG_INT and MPI_DOUBLE_INT.

All MPI objects (e.g., MPI_Datatype, MPI_Comm) are of type INTEGER in
Fortran.


NOTES ON COLLECTIVE OPERATIONS
------------------------------

The reduction operators ( MPI_Op ) do not return an error value. As a
result, if the functions detect an error, all they can do is either call
:ref:`MPI_Abort` or silently skip the problem. Thus, if you change the error
handler from MPI_ERRORS_ARE_FATAL to something else, for example,
MPI_ERRORS_RETURN , then no error may be indicated.

The reason for this is the performance problems in ensuring that all
collective routines return the same error value.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Allreduce`
   * :ref:`MPI_Reduce`
   * :ref:`MPI_Reduce_scatter`
   * :ref:`MPI_Scan`
   * :ref:`MPI_Op_create`
   * :ref:`MPI_Op_free`
