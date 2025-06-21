.. _mpi_init_thread:


MPI_Init_thread
===============

.. include_body

:ref:`MPI_Init_thread` |mdash| Initializes the MPI world model

.. The following file was automatically generated
.. include:: ./bindings/mpi_init_thread.rst

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

This routine, or :ref:`MPI_Init`, initializes the MPI world
model.  Either of these routines must be called before MPI
communication routines are called within the MPI world model.  The MPI
world model can be initialized at most exactly once in the lifetime of
an MPI process.  This is different than the MPI session model, which
can be initialized and finalized multiple times in an MPI process.
See :ref:`MPI_Session_init` and :ref:`MPI_Session_finalize`.

See `MPI-5.0:11.4.1 <https://www.mpi-forum.org/>`_ for a list of MPI
functionality that is available (e.g., even when the MPI
world model has not yet initialized or has already been finalized).

The MPI world model can be initialized at most once; subsequent calls
to :ref:`MPI_Init` or :ref:`MPI_Init_thread` are erroneous.

Alternatively, instead of the MPI world model, MPI applications can
use the sessions model; see :ref:`MPI_Session_init`.

Upon return, the level of thread support available to the program is
set in *provided*. In Open MPI, the value is dependent on how the
library was configured and built. Note that there is no guarantee that
*provided* will be greater than or equal to *required*.

Open MPI accepts the C *argc* and *argv* arguments to main, but
neither modifies, interprets, nor distributes them:

.. code-block:: c

   #include <mpi.h>

   int main(int argv, char *argv[]) {
       int provided;
       MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
       /* ...body of main MPI pogram... */
       MPI_Finalize();
       return 0;
   }


:ref:`MPI_Init_thread` has both a direct and an indirect mechanism to
request a specific level of thread support.  :ref:`MPI_Init` only has
an indirect mechanism to request a specific level of thread support.

Direct request of thread level
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:ref:`MPI_Init_thread` has the *required* parameter, which can be set
to any one of the following constants (from ``mpi.h``):

* ``MPI_THREAD_SINGLE``: Indicating that only one thread will execute.

* ``MPI_THREAD_FUNNELED``: Indicating that if the process is
  multithreaded, only the thread that called :ref:`MPI_Init_thread`
  will make MPI calls.

* ``MPI_THREAD_SERIALIZED``: Indicating that if the process is
  multithreaded, only one thread will make MPI library calls at one
  time.

* ``MPI_THREAD_MULTIPLE``: Indicating that if the process is
  multithreaded, multiple threads may call MPI at once with no
  restrictions.

The values of these constants adhere to the following relationships:

.. math::
   :nowrap:

   \begin{eqnarray}
       MPI\_THREAD\_SINGLE     & < & MPI\_THREAD\_FUNNELED \\
       MPI\_THREAD\_FUNNELED   & < & MPI\_THREAD\_SERIALIZED \\
       MPI\_THREAD\_SERIALIZED & < & MPI\_THREAD\_MULTIPLE \\
   \end{eqnarray}

Indirect request of thread level
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Both :ref:`MPI_Init_thread` and :ref:`MPI_Init` support an indirect
method of indicating the required thread level: setting the
``OMPI_MPI_THREAD_LEVEL`` environment variable:

* If the ``OMPI_MPI_THREAD_LEVEL`` environment variable is set at the
  time :ref:`MPI_Init` is invoked, it behaves as if
  :ref:`MPI_Init_thread` was invoked with the corresponding
  ``MPI_THREAD_*`` constant value passed via the *required* parameter.

* If the ``OMPI_MPI_THREAD_LEVEL`` environment variable is set at the
  time :ref:`MPI_Init_thread` is invoked, the ``MPI_THREAD_*``
  constant value corresponding to the environment variable value
  overrides the value passed via the *required* parameter.

The ``OMPI_MPI_THREAD_LEVEL`` environment variable can be set to any
of the values listed below.

.. list-table::
   :header-rows: 1

   * - Value that Open MPI uses
     - Allowable values (case-insensitive)

   * - ``MPI_THREAD_SINGLE``
     - ``MPI_THREAD_SINGLE``, ``SINGLE``, 0

   * - ``MPI_THREAD_FUNNELED``
     - ``MPI_THREAD_FUNNELED``, ``FUNNELED``, 1

   * - ``MPI_THREAD_SERIALIZED``
     - ``MPI_THREAD_SERIALIZED``, ``SERIALIZED``, 2

   * - ``MPI_THREAD_MULTIPLE``
     - ``MPI_THREAD_MULTIPLE``, ``MULTIPLE``, 3

.. note:: Prior to Open MPI v6.0.0, only the integer values 0 through
          3 were acceptable values for the ``OMPI_MPI_THREAD_LEVEL``
          environment variable.

          Starting with Open MPI v6.0.0, the Open MPI community
          recomends using one of the string name variants so that it
          can be correctly mapped to the corresponding Open MPI ABI
          value or the MPI Standard ABI value, as relevant.

NOTES
-----

The Fortran version does not have provisions for ``argc`` and ``argv`` and
takes only ``REQUIRED``, ``PROVIDED``, and ``IERROR``.

It is the caller's responsibility to check the value of ``provided``, as
it may be less than what was requested in ``required``.

The MPI Standard does not specify what a program using the MPI world
model can do before invoking :ref:`MPI_Init` or :ref:`MPI_Init_thread`
or after invoking :ref:`MPI_Finalize`. In the Open MPI implementation,
it should do as little as possible. In particular, avoid anything that
changes the external state of the program, such as opening files,
reading standard input, or writing to standard output.


MPI_THREAD_MULTIPLE Support
^^^^^^^^^^^^^^^^^^^^^^^^^^^

``MPI_THREAD_MULTIPLE`` support is included if the environment in which
Open MPI was built supports threading. You can check the output of
:ref:`ompi_info(1) <man1-ompi_info>` to see if Open MPI has
``MPI_THREAD_MULTIPLE`` support:

.. code-block:: bash

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
   * :ref:`MPI_Session_finalize`
   * :ref:`MPI_Session_init`
