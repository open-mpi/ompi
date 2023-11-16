.. _mpi_init_thread:


MPI_Init_thread
===============

.. include_body

:ref:`MPI_Init_thread` |mdash| Initializes the MPI execution environment


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   int MPI_Init_thread(int *argc, char ***argv,
   	int required, int *provided)


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   MPI_INIT_THREAD(REQUIRED, PROVIDED, IERROR)
   	INTEGER	REQUIRED, PROVIDED, IERROR


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   MPI_Init_thread(required, provided, ierror)
   	INTEGER, INTENT(IN) :: required
   	INTEGER, INTENT(OUT) :: provided
   	INTEGER, OPTIONAL, INTENT(OUT) :: ierror


INPUT PARAMETERS
----------------
* ``argc``: C only: Pointer to the number of arguments.
* ``argv``: C only: Argument vector.
* ``required``: Desired level of thread support (integer).

OUTPUT PARAMETERS
-----------------
* ``provided``: Available level of thread support (integer).
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

This routine, or :ref:`MPI_Init`, must be called before most other MPI routines
are called. There are a small number of exceptions, such as
:ref:`MPI_Initialized` and :ref:`MPI_Finalized`. MPI can be initialized at most once;
subsequent calls to :ref:`MPI_Init` or :ref:`MPI_Init_thread` are erroneous.

:ref:`MPI_Init_thread`, as compared to :ref:`MPI_Init`, has a provision to request a
certain level of thread support in *required*:

MPI_THREAD_SINGLE
   Only one thread will execute.

MPI_THREAD_FUNNELED
   If the process is multithreaded, only the thread that called
   :ref:`MPI_Init_thread` will make MPI calls.

MPI_THREAD_SERIALIZED
   If the process is multithreaded, only one thread will make MPI
   library calls at one time.

MPI_THREAD_MULTIPLE
   If the process is multithreaded, multiple threads may call MPI at
   once with no restrictions.

The level of thread support available to the program is set in
*provided*. In Open MPI, the value is dependent on how the library was
configured and built. Note that there is no guarantee that *provided*
will be greater than or equal to *required*.

Also note that calling :ref:`MPI_Init_thread` with a *required* value of
MPI_THREAD_SINGLE is equivalent to calling :ref:`MPI_Init`.

All MPI programs must contain a call to :ref:`MPI_Init` or :ref:`MPI_Init_thread`.
Open MPI accepts the C *argc* and *argv* arguments to main, but neither
modifies, interprets, nor distributes them:

.. code-block:: c

   /* declare variables */
   MPI_Init_thread(&argc, &argv, req, &prov);
   /* parse arguments */
   /* main program */
   MPI_Finalize();


NOTES
-----

The Fortran version does not have provisions for ``argc`` and ``argv`` and
takes only ``IERROR``.

It is the caller's responsibility to check the value of ``provided``, as
it may be less than what was requested in ``required``.

The MPI Standard does not say what a program can do before an
:ref:`MPI_Init_thread` or after an :ref:`MPI_Finalize`. In the Open MPI
implementation, it should do as little as possible. In particular, avoid
anything that changes the external state of the program, such as opening
files, reading standard input, or writing to standard output.


MPI_THREAD_MULTIPLE Support
^^^^^^^^^^^^^^^^^^^^^^^^^^^

``MPI_THREAD_MULTIPLE`` support is included if the environment in which
Open MPI was built supports threading. You can check the output of
:ref:`ompi_info(1) <man1-ompi_info>` to see if Open MPI has
``MPI_THREAD_MULTIPLE`` support:

::

   shell$ ompi_info | grep "Thread support"
             Thread support: posix (MPI_THREAD_MULTIPLE: yes, OPAL support: yes, OMPI progress: no, Event lib: yes)
   shell$

The ``MPI_THREAD_MULTIPLE: yes`` portion of the above output indicates
that Open MPI was compiled with ``MPI_THREAD_MULTIPLE`` support.

Note that there is a small performance penalty for using
``MPI_THREAD_MULTIPLE`` support; latencies for short messages will be higher
as compared to when using ``MPI_THREAD_SINGLE``, for example.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init`
   * :ref:`MPI_Initialized`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
