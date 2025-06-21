.. _mpi_init:


MPI_Init
========

.. include_body

:ref:`MPI_Init` |mdash| Initializes the MPI world model

.. The following file was automatically generated
.. include:: ./bindings/mpi_init.rst

INPUT PARAMETERS
----------------
* ``argc``: C only: Pointer to the number of arguments.
* ``argv``: C only: Argument vector.

OUTPUT PARAMETER
----------------
* ``ierror``: Fortran only: Error status (integer).

DESCRIPTION
-----------

When using the MPI world model, this routine, or
:ref:`MPI_Init_thread`, must be called before most other MPI routines
are called. There are a small number of routines that can be invoked
before :ref:`MPI_Init` or :ref:`MPI_Init_thread`, such as
:ref:`MPI_Initialized` and :ref:`MPI_Finalized`.

The MPI world model can be initialized at most once; subsequent calls
to :ref:`MPI_Init` or :ref:`MPI_Init_thread` are erroneous.

Alternatively, instead of the MPI world model, MPI applications can
use the sessions model; see :ref:`MPI_Session_init`.

Open MPI accepts the C *argc* and *argv* arguments to main, but neither
modifies, interprets, nor distributes them:

.. code-block:: c

   #include <mpi.h>

   int main(int argv, char *argv[]) {
       MPI_Init(&argc, &argv);
       /* ...body of main MPI pogram... */
       MPI_Finalize();
       return 0;
   }

By default, :ref:`MPI_Init` is effectively equivalent to invoking
:ref:`MPI_Init_thread` with a *required* value of
``MPI_THREAD_SINGLE``.  However, if the ``OMPI_MPI_THREAD_LEVEL``
environment variable is set to a valid value when :ref:`MPI_Init` is
invoked, it is equivalent to invoking :ref:`MPI_Init_thread` with
*required* set to the integer value of the ``OMPI_MPI_THREAD_LEVEL``
environment variable.  See :ref:`MPI_Init_thread` for more details.

NOTES
-----

The Fortran version does not have provisions for *argc* and *argv* and
takes only IERROR.

The MPI Standard does not specify what a program using the MPI world
model can do before invoking :ref:`MPI_Init` or :ref:`MPI_Init_thread`
or after invoking :ref:`MPI_Finalize`. In the Open MPI implementation,
it should do as little as possible. In particular, avoid anything that
changes the external state of the program, such as opening files,
reading standard input, or writing to standard output.


ERRORS
------

.. include:: ./ERRORS.rst

.. seealso::
   * :ref:`MPI_Init_thread`
   * :ref:`MPI_Initialized`
   * :ref:`MPI_Finalize`
   * :ref:`MPI_Finalized`
   * :ref:`MPI_Session_finalize`
   * :ref:`MPI_Session_init`
