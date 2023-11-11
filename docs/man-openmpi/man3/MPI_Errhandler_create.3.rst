.. _mpi_errhandler_create:


MPI_Errhandler_create
=====================

.. include_body

:ref:`MPI_Errhandler_create` |mdash| Creates an MPI-style error handler |mdash| |deprecated_favor| :ref:`MPI_Comm_create_errhandler`.



SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Errhandler_create(MPI_Handler_function *function,
   	MPI_Errhandler *errhandler)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'

   MPI_ERRHANDLER_CREATE(FUNCTION, ERRHANDLER, IERROR)
   	EXTERNAL	FUNCTION
   	INTEGER	ERRHANDLER, IERROR


INPUT PARAMETER
---------------

* ``function``: User-defined error handling procedure.

OUTPUT PARAMETERS
-----------------

* ``errhandler``: MPI error handler (handle).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

Note that use of this routine is *deprecated* as of MPI-2. Please use
:ref:`MPI_Comm_create_errhandler` instead.

Registers the user routine function for use as an MPI error handler.
Returns in errhandler a handle to the registered error handler.

In the C language, the user routine should be a C function of type
MPI_Handler_function, which is defined as

.. code-block:: c

       typedef void (MPI_Handler_function)(MPI_Comm *, int *, ...);

The first argument is the communicator in use. The second is the error
code to be returned by the MPI routine that raised the error. If the
routine would have returned MPI_ERR_IN_STATUS, it is the error code
returned in the status for the request that caused the error handler to
be invoked. The remaining arguments are stdargs arguments whose number
and meaning is implementation-dependent. An implementation should
clearly document these arguments. Addresses are used so that the handler
may be written in Fortran.


NOTE
----

The MPI-1 Standard states that an implementation may make the output
value (errhandler) simply the address of the function. However, the
action of :ref:`MPI_Errhandler_free` makes this impossible, since it is
required to set the value of the argument to MPI_ERRHANDLER_NULL. In
addition, the actual error handler must remain until all communicators
that use it are freed.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Comm_create_errhandler`
   * :ref:`MPI_Comm_get_errhandler`
   * :ref:`MPI_Comm_set_errhandler`
