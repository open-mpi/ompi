Using Memchecker
================

The Memchecker functionality in Open MPI provides MPI semantic
checking for your application (as well as internals of Open MPI), with
the help of memory checking tools such as the ``memcheck`` component of
`the Valgrind suite <https://www.valgrind.org/>`_.

/////////////////////////////////////////////////////////////////////////

Types of Errors Detected by Memchecker
--------------------------------------

Open MPI's Memchecker is based on the ``memcheck`` tool included with
Valgrind, so it takes all the advantages from it. Firstly, it checks
all reads and writes of memory, and intercepts calls to
``malloc(3)``/``free(3)`` and C++'s ``new``/``delete`` operators.
Most importantly, Memchecker is able to detect
the user buffer errors in both non-blocking and one-sided
communications, e.g. reading or writing to buffers of active
non-blocking receive operations and writing to buffers of active
non-blocking send operations.

Here are some example problems that Memchecker can detect:

Accessing buffer under control of non-blocking communication:

.. code-block:: c

   int buf;
   MPI_Irecv(&buf, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &req);
   // The following line will produce a memchecker warning
   buf = 4711;
   MPI_Wait (&req, &status);

Wrong input parameters, e.g., wrong-sized send buffers:

.. code-block:: c

   char *send_buffer;
   send_buffer = malloc(5);
   memset(send_buffer, 0, 5);
   // The following line will produce a memchecker warning
   MPI_Send(send_buffer, 10, MPI_CHAR, 1, 0, MPI_COMM_WORLD);

Accessing a window in a one-sided communication:

.. code-block:: c

   MPI_Get(A, 10, MPI_INT, 1, 0, 1, MPI_INT, win);
   A[0] = 4711;
   MPI_Win_fence(0, win);

Uninitialized input buffers:

.. code-block:: c

   char *buffer;
   buffer = malloc(10);
   // The following line will produce a memchecker warning
   MPI_Send(buffer, 10, MPI_INT, 1, 0, MPI_COMM_WORLD);

Usage of the uninitialized ``MPI_Status`` field in ``MPI_ERROR``
structure: (the MPI-1 standard defines the ``MPI ERROR`` field to be
undefined for single-completion calls such as :ref:`MPI_Wait(3) <mpi_wait>` or
:ref:`MPI_Test(3) <mpi_test>`, see MPI-1 p. 22):

.. code-block:: c

   MPI_Wait(&request, &status);
   // The following line will produce a memchecker warning
   if (status.MPI_ERROR != MPI_SUCCESS)
       return ERROR;

/////////////////////////////////////////////////////////////////////////

Building Open MPI with Memchecker Support
-----------------------------------------

To use Memchecker, you need Valgrind 3.2.0 or later, and have an Open
MPI that was configured with the ``--enable-memchecker`` and
``--enable-debug`` flags.

.. note:: The Memchecker functionality is off by default, because it
          incurs a performance penalty.

When ``--enable-memchecker`` is specified, ``configure`` will check
for a recent-enable valgrind distribution.  If found, Open MPI will
build Memchecker support.

For example:

.. code-block:: sh

   shell$ ./configure --prefix=/path/to/openmpi --enable-debug \
       --enable-memchecker --with-valgrind=/path/to/valgrind

You can check that Open MPI was built with Memchecker support by using
the :ref:`ompi_info(1) <man1-ompi_info>` command.

.. code-block:: sh

   # The exact version numbers shown may be different for your Open
   # MPI installation
   shell$ ompi_info | grep memchecker
   MCA memchecker: valgrind (MCA v1.0, API v1.0, Component v1.3)

If you do not see the "MCA memchecker: valgrind" line, you probably
did not configure and install Open MPI correctly.

/////////////////////////////////////////////////////////////////////////

Running an Open MPI Application with Memchecker
-----------------------------------------------

After Open MPI was built and installed with Memchecker support, 
simply run your application with Valgrind, e.g.:

.. code-block:: sh

   shell$ mpirun -n 2 valgrind ./my_app

If you enabled Memchecker, but you don't want to check the
application at this time, then just run your application as
usual. E.g.:

.. code-block:: sh

   shell$ mpirun -n 2 ./my_app

/////////////////////////////////////////////////////////////////////////

Application Performance Impacts Using Memchecker
------------------------------------------------

The configure option ``--enable-memchecker`` (together with
``--enable-debug``) *does* cause performance degradation, even if not
running under Valgrind.  The following explains the mechanism and may
help in making the decision whether to provide a cluster-wide
installation with ``--enable-memchecker``.

There are two cases:

#. If run without Valgrind, the Valgrind ClientRequests (assembler
   instructions added to the normal execution path for checking) do
   not affect overall MPI performance. Valgrind ClientRequests are
   explained in detail `in Valgrind's documentation
   <https://valgrind.org/docs/manual/manual-core-adv.html>`_.
   In the case of x86-64, ClientRequests boil down to the following
   four rotate-left (ROL) and one xchange (XCHG) assembler instructions
   from ``valgrind.h``:

   .. code-block:: c

      #define __SPECIAL_INSTRUCTION_PREAMBLE                      \
                     "rolq \$3,  %%rdi; rolq \$13, %%rdi\\n\\t"   \
                     "rolq \$61, %%rdi; rolq \$51, %%rdi\\n\\t"

   and

   .. We do not make the code block below as "c" because the Sphinx C
      syntax highlighter fails to parse it as C and emits a warning.
      So we might as well just leave it as a plan verbatim block
      (i.e., not syntax highlighted).

   .. code-block::

      __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %RDX = client_request ( %RAX ) */           \
                     "xchgq %%rbx,%%rbx"                            \
                     : "=d" (_zzq_result)                           \
                     : "a" (& _zzq_args``0``), "0" (_zzq_default)   \
                     : "cc", "memory"                               \
                    );

   for every single ClientRequest.  In the case of not running
   Valgrind, these ClientRequest instructions do not change the
   arithmetic outcome (rotating a 64-bit register left by 128-Bits,
   exchanging a register with itself), except for the carry flag.

   The first request is checking whether we're running under Valgrind.
   In case we're not running under Valgrind subsequent checks (a.k.a.
   ClientRequests) are not done.

#. If the application is run under Valgrind, performance is naturally reduced due
   to the Valgrind JIT and the checking tool employed.
   For costs and overheads of Valgrind's Memcheck tool on the SPEC 2000 Benchmark,
   please see the excellent paper
   `Valgrind: A Framework for Heavyweight Dynamic Binary Instrumentation
   <https://valgrind.org/docs/valgrind2007.pdf>`_.
   For an evaluation of various internal implementation alternatives of Shadow Memory, please see
   `Building Workload Characterization Tools with Valgrind
   <https://valgrind.org/docs/iiswc2006.pdf>`_.
