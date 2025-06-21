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

This routine, or :ref:`MPI_Init_thread`, initializes the MPI world
model.  Either of these routines must be called before MPI
communication routines are called within the MPI world model.  The MPI
world model can be initialized at most exactly once in the lifetime of
an MPI process.  This is different than the MPI session model, which
can be initialized and finalized multiple times in an MPI process.
See :ref:`MPI_Session_init` and :ref:`MPI_Session_finalize`.

See `MPI-5.0:11.4.1 <https://www.mpi-forum.org/>`_ for a list of MPI
functionality that is available (e.g., even when the MPI
world model has not yet initialized or has already been finalized).

Open MPI's :ref:`MPI_Init` and :ref:`MPI_Init_thread` both accept the
C *argc* and *argv* arguments to main, but neither modifies,
interprets, nor distributes them:

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
*required* set to the corresponding value of the ``OMPI_MPI_THREAD_LEVEL``
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
