.. _mpi_op_create:


MPI_Op_create
=============

.. include_body

:ref:`MPI_Op_create` |mdash| Creates a user-defined combination function handle.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Op_create(MPI_User_function *function, int commute,
   	MPI_Op *op)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_OP_CREATE(FUNCTION, COMMUTE, OP, IERROR)
   	EXTERNAL	FUNCTION
   	LOGICAL	COMMUTE
   	INTEGER	OP, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Op_create(user_fn, commute, op, ierror)
   	PROCEDURE(MPI_User_function) :: user_fn
   	LOGICAL, INTENT(IN) :: commute
   	TYPE(MPI_Op), INTENT(OUT) :: op
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``function``: User-defined function (function).
* ``commute``: True if commutative; false otherwise.

OUTPUT PARAMETERS
-----------------
* ``op``: Operation (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Op_create` binds a user-defined global operation to an op handle that
can subsequently be used in :ref:`MPI_Reduce`, :ref:`MPI_Allreduce`,
:ref:`MPI_Reduce_scatter`, and :ref:`MPI_Scan`. The user-defined operation is assumed
to be associative. If commute = true, then the operation should be both
commutative and associative. If commute = false, then the order of
operands is fixed and is defined to be in ascending, process rank order,
beginning with process zero. The order of evaluation can be changed,
taking advantage of the associativity of the operation. If commute =
true then the order of evaluation can be changed, taking advantage of
commutativity and associativity.

*function* is the user-defined function, which must have the following
four arguments: invec, inoutvec, len, and datatype.

The ANSI-C prototype for the function is the following:

.. code-block:: c

     typedef void MPI_User_function(void *invec, void *inoutvec,
                                    int *len,
                                    MPI_Datatype *datatype);

The Fortran declaration of the user-defined function appears below.

.. code-block:: fortran

     FUNCTION USER_FUNCTION( INVEC(*), INOUTVEC(*), LEN, TYPE)
     <type> INVEC(LEN), INOUTVEC(LEN)
      INTEGER LEN, TYPE

The datatype argument is a handle to the data type that was passed into
the call to :ref:`MPI_Reduce`. The user reduce function should be written such
that the following holds: Let u[0], ..., u[len-1] be the len elements in
the communication buffer described by the arguments invec, len, and
datatype when the function is invoked; let v[0], ..., v[len-1] be len
elements in the communication buffer described by the arguments
inoutvec, len, and datatype when the function is invoked; let w[0], ...,
w[len-1] be len elements in the communication buffer described by the
arguments inoutvec, len, and datatype when the function returns; then
w[i] = u[i] o v[i], for i=0 ,..., len-1, where o is the reduce operation
that the function computes.

Informally, we can think of invec and inoutvec as arrays of len elements
that function is combining. The result of the reduction over-writes
values in inoutvec, hence the name. Each invocation of the function
results in the pointwise evaluation of the reduce operator on len
elements: i.e, the function returns in inoutvec[i] the value invec[i] o
inoutvec[i], for i = 0..., count-1, where o is the combining operation
computed by the function.

By internally comparing the value of the datatype argument to known,
global handles, it is possible to overload the use of a single
user-defined function for several different data types.

General datatypes may be passed to the user function. However, use of
datatypes that are not contiguous is likely to lead to inefficiencies.

No MPI communication function may be called inside the user function.
:ref:`MPI_Abort` may be called inside the function in case of an error.


NOTES
-----

Suppose one defines a library of user-defined reduce functions that are
overloaded: The datatype argument is used to select the right execution
path at each invocation, according to the types of the operands. The
user-defined reduce function cannot "decode" the datatype argument that
it is passed, and cannot identify, by itself, the correspondence between
the datatype handles and the datatype they represent. This
correspondence was established when the datatypes were created. Before
the library is used, a library initialization preamble must be executed.
This preamble code will define the datatypes that are used by the
library and store handles to these datatypes in global, static variables
that are shared by the user code and the library code.

**Example:** Example of user-defined reduce:

Compute the product of an array of complex numbers, in C.

.. code-block:: c

       typedef struct {
           double real,imag;
       } Complex;

       /* the user-defined function
        */
       void myProd( Complex *in, Complex *inout, int *len,
                    MPI_Datatype *dptr )
       {
           int i;
           Complex c;

       for (i=0; i< *len; ++i) {
               c.real = inout->real*in->real -
                          inout->imag*in->imag;
               c.imag = inout->real*in->imag +
                          inout->imag*in->real;
               *inout = c;
               in++; inout++;
           }
       }

       /* and, to call it...
        */
       ...

       /* each process has an array of 100 Complexes
            */
           Complex a[100], answer[100];
           MPI_Op myOp;
           MPI_Datatype ctype;

       /* explain to MPI how type Complex is defined
            */
          MPI_Type_contiguous( 2, MPI_DOUBLE, &ctype );
           MPI_Type_commit( &ctype );
           /* create the complex-product user-op
            */
           MPI_Op_create( myProd, True, &myOp );

           MPI_Reduce( a, answer, 100, ctype, myOp, root, comm );

           /* At this point, the answer, which consists of 100 Complexes,
            * resides on process root
            */

The Fortran version of :ref:`MPI_Reduce` will invoke a user-defined reduce
function using the Fortran calling conventions and will pass a
Fortran-type datatype argument; the C version will use C calling
convention and the C representation of a datatype handle. Users who plan
to mix languages should define their reduction functions accordingly.


NOTES ON COLLECTIVE OPERATIONS
------------------------------

The reduction functions ( MPI_Op ) do not return an error value. As a
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
   * :ref:`MPI_Reduce`
   * :ref:`MPI_Reduce_scatter`
   * :ref:`MPI_Allreduce`
   * :ref:`MPI_Scan`
   * :ref:`MPI_Op_free`
