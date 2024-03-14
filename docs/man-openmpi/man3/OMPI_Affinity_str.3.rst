.. _ompi_affinity_str:


OMPI_Affinity_str
=================

.. include_body

**OMPI_Affinity_str** - Obtain prettyprint strings of processor affinity
information for this process


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>
   #include <mpi-ext.h>

   int OMPI_Affinity_str(ompi_affinity_fmt_type_t fmt_type,
                         char ompi_bound[OMPI_AFFINITY_STRING_MAX],
                         char current_binding[OMPI_AFFINITY_STRING_MAX],
                         char exists[OMPI_AFFINITY_STRING_MAX])


Fortran Syntax
^^^^^^^^^^^^^^

There is no Fortran binding for this function.


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

There is no Fortran 2008 binding for this function.


C++ Syntax
^^^^^^^^^^

There is no C++ binding for this function.


INPUT PARAMETERS
----------------
* ``fmt_type``: An enum indicating how to format the returned ompi_bound and current_binding strings. OMPI_AFFINITY_RSRC_STRING_FMT returns the string as human-readable resource names, such as "socket 0, core 0".
* ``OMPI_AFFINITY_LAYOUT_FMT returns ASCII art representing where this MPI``: process is bound relative to the machine resource layout. For example "[. B][. .]" shows the process that called the routine is bound to socket 0, core 1 in a system with 2 sockets, each containing 2 cores.
* ``See below for more output examples.``:

OUTPUT PARAMETERS
-----------------
* ``ompi_bound``: A prettyprint string describing what processor(s) Open MPI bound this process to, or a string indicating that Open MPI did not bind this process.
* ``current_binding``: A prettyprint string describing what processor(s) this process is currently bound to, or a string indicating that the process is bound to all available processors (and is therefore considered "unbound").
* ``exists``: A prettyprint string describing the available sockets and sockets on this host.

DESCRIPTION
-----------

Open MPI may bind a process to specific sockets and/or cores at process
launch time. This non-standard Open MPI function call returns
prettyprint information about three things:

Where Open MPI bound this process.
   The string returned in **ompi_bound** will either indicate that Open
   MPI did not bind this process to anything, or it will contain a
   prettyprint description of the processor(s) to which Open MPI bound
   this process.

Where this process is currently bound.
   Regardless of whether Open MPI bound this process or not, another
   entity may have bound it. The string returned in **current_binding**
   will indicate what the *current* binding is of this process,
   regardless of what Open MPI may have done earlier. The string
   returned will either indicate that the process is unbound (meaning
   that it is bound to all available processors) or it will contain a
   prettyprint description of the sockets and cores to which the process
   is currently bound.

What processors exist.
   As a convenience to the user, the **exists** string will contain a
   prettyprint description of the sockets and cores that this process
   can see (which is *usually* all processors in the system).


EXAMPLES
^^^^^^^^

**Example 1:** Print out processes binding using resource string format.

.. code-block:: c

       int rank;
       char ompi_bound[OMPI_AFFINITY_STRING_MAX];
       char current_binding[OMPI_AFFINITY_STRING_MAX];
       char exists[OMPI_AFFINITY_STRING_MAX];

       MPI_Init(&argc, &argv);
       MPI_Comm_rank(MPI_COMM_WORLD, &rank);

       OMPI_Affinity_str(OMPI_AFFINITY_RSRC_STRING_FMT,
                         ompi_bound, current_binding, exists);
       printf("rank %d: \n"
              "       ompi_bound: %s\n"
              "  current_binding: %s\n"
              "           exists: %s\n",
              rank, ompi_bound, current_binding, exists);
       ...

Output of ``mpirun -n 2 -bind-to-core a.out``:

::

   rank 0:
          ompi_bound: socket 0[core 0]
     current_binding: socket 0[core 0]
              exists: socket 0 has 4 cores
   rank 1:
          ompi_bound: socket 0[core 1]
     current_binding: socket 0[core 1]
              exists: socket 0 has 4 cores

Output of ``mpirun -n 2 -bind-to-socket a.out``:

::

   rank 0:
          ompi_bound: socket 0[core 0-3]
     current_binding: Not bound (or bound to all available processors)
              exists: socket 0 has 4 cores
   rank 1:
          ompi_bound: socket 0[core 0-3]
     current_binding: Not bound (or bound to all available processors)
              exists: socket 0 has 4 cores

|
| **Example 2:** Print out processes binding using layout string format.

.. code-block:: c

       int rank;
       char ompi_bound[OMPI_AFFINITY_STRING_MAX];
       char current_binding[OMPI_AFFINITY_STRING_MAX];
       char exists[OMPI_AFFINITY_STRING_MAX];

       MPI_Init(&argc, &argv);
       MPI_Comm_rank(MPI_COMM_WORLD, &rank);

       OMPI_Affinity_str(OMPI_AFFINITY_LAYOUT_FMT,
                         ompi_bound, current_binding, exists);
       printf("rank %d: \n"
              "       ompi_bound: %s\n"
              "  current_binding: %s\n"
              "           exists: %s\n",
              rank, ompi_bound, current_binding, exists);
       ...

Output of ``mpirun -n 2 -bind-to-core a.out``:

::

   rank 0:
          ompi_bound: [B . . .]
     current_binding: [B . . .]
              exists: [. . . .]
   rank 1:
          ompi_bound: [. B . .]
     current_binding: [. B . .]
              exists: [. . . .]

Output of ``mpirun -n 2 -bind-to-socket a.out``:

::

   rank 0:
          ompi_bound: [B B B B]
     current_binding: [B B B B]
              exists: [. . . .]
   rank 1:
          ompi_bound: [B B B B]
     current_binding: [B B B B]
              exists: [. . . .]


.. seealso:: :ref:`mpirun(1) <man1-mpirun>`
