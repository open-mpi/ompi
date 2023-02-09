.. _mpi_scan:


MPI_Scan
========

.. include_body

:ref:`MPI_Scan`, :ref:`MPI_Iscan`, :ref:`MPI_Scan_init` - Computes an inclusive scan
(partial reduction)


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Scan(const void *sendbuf, void *recvbuf, int count,
                MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)

   int MPI_Iscan(const void *sendbuf, void *recvbuf, int count,
                 MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,
                 MPI_Request *request)

   int MPI_Scan_init(const void *sendbuf, void *recvbuf, int count,
                 MPI_Datatype datatype, MPI_Op op, MPI_Comm comm,
                 MPI_Info info, MPI_Request *request)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_SCAN(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	COUNT, DATATYPE, OP, COMM, IERROR

   MPI_ISCAN(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, REQUEST, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	COUNT, DATATYPE, OP, COMM, REQUEST, IERROR

   MPI_SCAN_INIT(SENDBUF, RECVBUF, COUNT, DATATYPE, OP, COMM, INFO, REQUEST, IERROR)
   	<type>	SENDBUF(*), RECVBUF(*)
   	INTEGER	COUNT, DATATYPE, OP, COMM, INFO, REQUEST, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Scan(sendbuf, recvbuf, count, datatype, op, comm, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
   	TYPE(*), DIMENSION(..) :: recvbuf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Iscan(sendbuf, recvbuf, count, datatype, op, comm, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror

   MPI_Scan_init(sendbuf, recvbuf, count, datatype, op, comm, info, request, ierror)
   	TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
   	TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
   	INTEGER, INTENT(IN) :: count
   	TYPE(MPI_Datatype), INTENT(IN) :: datatype
   	TYPE(MPI_Op), INTENT(IN) :: op
   	TYPE(MPI_Comm), INTENT(IN) :: comm
   	TYPE(MPI_Info), INTENT(IN) :: info
   	TYPE(MPI_Request), INTENT(OUT) :: request
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``sendbuf``: Send buffer (choice).
* ``count``: Number of elements in input buffer (integer).
* ``datatype``: Data type of elements of input buffer (handle).
* ``op``: Operation (handle).
* ``comm``: Communicator (handle).
* ``info``: Info (handle, persistent only)

OUTPUT PARAMETERS
-----------------
* ``recvbuf``: Receive buffer (choice).
* ``request``: Request (handle, non-blocking only).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

:ref:`MPI_Scan` is used to perform an inclusive prefix reduction on data
distributed across the calling processes. The operation returns, in the
*recvbuf* of the process with rank i, the reduction (calculated
according to the function *op*) of the values in the *sendbuf*\ s of
processes with ranks 0, ..., i (inclusive). The type of operations
supported, their semantics, and the constraints on send and receive
buffers are as for :ref:`MPI_Reduce`.


EXAMPLE
-------

This example uses a user-defined operation to produce a segmented scan.
A segmented scan takes, as input, a set of values and a set of logicals,
where the logicals delineate the various segments of the scan. For
example,

::

   values     v1      v2      v3      v4      v5      v6      v7      v8
   logicals   0       0       1       1       1       0       0       1
   result     v1    v1+v2     v3    v3+v4  v3+v4+v5   v6    v6+v7     v8

The result for rank j is thus the sum v(i) + ... + v(j), where i is the
lowest rank such that for all ranks n, i <= n <= j, logical(n) =
logical(j). The operator that produces this effect is

::

         [ u ]     [ v ]     [ w ]
         [   ]  o  [   ]  =  [   ]
         [ i ]     [ j ]     [ j ]

   where

       ( u + v if i = j w = ( ( v if i != j

Note that this is a noncommutative operator. C code that implements it
is given below.

.. code-block:: c

   	typedef struct {
   		double val;
   		int log;
   	} SegScanPair;

   	/*
   	 * the user-defined function
   	 */
   	void segScan(SegScanPair *in, SegScanPair *inout, int *len,
   		MPI_Datatype *dptr)
   	{
   		int i;
   		SegScanPair c;

   		for (i = 0; i < *len; ++i) {
   			if (in->log == inout->log)
   				c.val = in->val + inout->val;
   			else
   				c.val = inout->val;

   			c.log = inout->log;
   			*inout = c;
   			in++;
   			inout++;
   		}
   	}

Note that the inout argument to the user-defined function corresponds to
the right-hand operand of the operator. When using this operator, we
must be careful to specify that it is noncommutative, as in the
following:

.. code-block:: c

   	int			i, base;
   	SeqScanPair	a, answer;
   	MPI_Op		myOp;
   	MPI_Datatype	type[2] = {MPI_DOUBLE, MPI_INT};
   	MPI_Aint		disp[2];
   	int			blocklen[2] = {1, 1};
   	MPI_Datatype	sspair;

   	/*
   	 * explain to MPI how type SegScanPair is defined
   	 */
   	MPI_Get_address(a, disp);
   	MPI_Get_address(a.log, disp + 1);
   	base = disp[0];
   	for (i = 0; i < 2; ++i)
   		disp[i] -= base;
   	MPI_Type_struct(2, blocklen, disp, type, &sspair);
   	MPI_Type_commit(&sspair);

   	/*
   	 * create the segmented-scan user-op
   	 * noncommutative - set commute (arg 2) to 0
   	 */
   	MPI_Op_create((MPI_User_function *)segScan, 0, &myOp);
   	...
   	MPI_Scan(a, answer, 1, sspair, myOp, comm);


USE OF IN-PLACE OPTION
----------------------

When the communicator is an intracommunicator, you can perform a
scanning operation in place (the output buffer is used as the input
buffer). Use the variable MPI_IN_PLACE as the value of the *sendbuf*
argument. The input data is taken from the receive buffer and replaced
by the output data.


NOTES ON COLLECTIVE OPERATIONS
------------------------------

The reduction functions of type MPI_Op do not return an error value. As
a result, if the functions detect an error, all they can do is either
call :ref:`MPI_Abort` or silently skip the problem. Thus, if the error handler
is changed from MPI_ERRORS_ARE_FATAL to something else (e.g.,
MPI_ERRORS_RETURN), then no error may be indicated.

The reason for this is the performance problems in ensuring that all
collective routines return the same error value.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Exscan`
   * :ref:`MPI_Op_create`
   * :ref:`MPI_Reduce`
