Parallel debugging
==================

.. TODO How can I create a TOC just for this page here at the top?

//////////////////////////////////////////////////////////

My process dies without any output.  Why?
-----------------------------------------

There many be many reasons for this; the Open MPI Team strongly
encourages the use of tools (such as debuggers) whenever possible.

One of the reasons, however, may come from inside Open MPI itself.  If
your application fails due to memory corruption, Open MPI may
subsequently fail to output an error message before dying.
Specifically, starting with v1.3, Open MPI attempts to aggregate error
messages from multiple processes in an attempt to show unique error
messages only once (vs. one for each MPI process |mdash| which can be
unwieldy, especially when running large MPI jobs).

However, this aggregation process requires allocating memory in the
MPI process when it displays the error message.  If the process's
memory is already corrupted, Open MPI's attempt to allocate memory may
fail and the process will simply die, possibly silently.  When Open
MPI does not attempt to aggregate error messages, most of its setup
work is done during MPI_INIT and no memory is allocated during the
"print the error" routine.  It therefore almost always successfully
outputs error messages in real time |mdash| but at the expense that you'll
potentially see the same error message for *each* MPI process that
encountered the error.

Hence, the error message aggregation is _usually_ a good thing, but
sometimes it can mask a real error.  You can disable Open MPI's error
message aggregation with the ``orte_base_help_aggregate`` MCA
parameter.  For example:

.. code-block:: sh

   shell$ mpirun --mca orte_base_help_aggregate 0 ...

//////////////////////////////////////////////////////////

What is Memchecker?
-------------------

The Memchecker  allows MPI semantic
checking for your application (as well as internals of Open MPI), with
the help of memory checking tools such as the Memcheck of `the
Valgrind suite <https://www.valgrind.org/>`_.

/////////////////////////////////////////////////////////////////////////

What kind of errors can Memchecker find?
----------------------------------------

Memchecker is implemented on the basis of the Memcheck tool from
Valgrind, so it takes all the advantages from it. Firstly, it checks
all reads and writes of memory, and intercepts calls to
malloc/new/free/delete. Most importantly, Memchecker is able to detect
the user buffer errors in both Non-blocking and One-sided
communications, e.g. reading or writing to buffers of active
non-blocking Recv-operations and writing to buffers of active
non-blocking Send-operations.

Here are some example codes that Memchecker can detect:

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
undefined for single-completion calls such as ``MPI_WAIT`` or
``MPI_TEST``, see MPI-1 p. 22):

.. code-block:: c

   MPI_Wait(&request, &status);
   // The following line will produce a memchecker warning
   if (status.MPI_ERROR != MPI_SUCCESS)
       return ERROR;

/////////////////////////////////////////////////////////////////////////

How do I build Open MPI with Memchecker support?
------------------------------------------------

To use Memchecker, you need Valgrind 3.2.0 or later, and have an Open
MPI that was configured with the ``--enable-memchecker`` and
``--enable-debug`` flags.

.. note:: The Memchecker functionality is off by default, because it
          incurs a performance penalty.

When ``--enable-memchecker`` is specified, ``configure`` will check
for a recent-enable Valgrind distribution.  If found, Open MPI will
build Memchecker support.

For example:

.. code-block:: sh

   shell$ ./configure --prefix=/path/to/openmpi --enable-debug \
       --enable-memchecker --with-valgrind=/path/to/valgrind

You can check that Open MPI was built with Memchecker support by using
the ``ompi_info`` application:

.. code-block:: sh

   # The exact version numbers shown may be different for your Open
   # MPI installation
   shell$ ompi_info | grep memchecker
   MCA memchecker: valgrind (MCA v1.0, API v1.0, Component v1.3)

If you do not see the "MCA memchecker: valgrind" line, ou probably
didn't configure and install Open MPI correctly.

/////////////////////////////////////////////////////////////////////////

How to run my MPI application with Memchecker?
----------------------------------------------

First of all, you have to make sure that Valgrind 3.2.0 or later is
installed, and Open MPI is compiled with Memchecker support
enabled. Then simply run your application with Valgrind, e.g.:

.. code-block:: sh

   shell$ mpirun -n 2 valgrind ./my_app

Or if you enabled Memchecker, but you don't want to check the
application at this time, then just run your application as
usual. E.g.:

.. code-block:: sh

   shell$ mpirun -n 2 ./my_app

/////////////////////////////////////////////////////////////////////////

Does Memchecker cause performance degradation to my application?
----------------------------------------------------------------

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
   In case we're not running under Valgrind subsequent checks (aka ClientRequests)
   are not done.

#. If the application is run under Valgrind, performance is naturally reduced due
   to the Valgrind JIT and the checking tool employed.
   For costs and overheads of Valgrind's Memcheck tool on the SPEC 2000 Benchmark,
   please see the excellent paper
   `Valgrind: A Framework for Heavyweight Dynamic Binary Instrumentation
   <https://valgrind.org/docs/valgrind2007.pdf>`_.
   For an evaluation of various internal implementation alternatives of Shadow Memory, please see
   `Building Workload Characterization Tools with Valgrind
   <https://valgrind.org/docs/iiswc2006.pdf>`_.


Further information and performance data with the NAS Parallel
Benchmarks may be found in the paper `Memory Debugging of MPI-Parallel
Applications in Open MPI
<https://www.open-mpi.org/papers/parco-2007/>`_.

/////////////////////////////////////////////////////////////////////////

Is Open MPI "Valgrind-clean" or how can I identify real errors?
---------------------------------------------------------------

This issue has been raised many times on the mailing list, e.g., `such
as here
<https://www.open-mpi.org/community/lists/users/2007/05/3192.php>`_
`and here
<https://www.open-mpi.org/community/lists/users/2009/06/9565.php>`_.

There are many situations where Open MPI purposefully does not initialize and
subsequently communicates memory, e.g., by calling ``writev(2)``.
Furthermore, several cases are known where memory is not properly freed upon
``MPI_FINALIZE``.

This certainly does not help distinguishing real errors from false positives.
Valgrind provides functionality to suppress errors and warnings from certain
function contexts.

In an attempt to ease debugging using Valgrind, Open MPI provides a
so-called Valgrind-suppression file, that can be passed on the command
line:

.. code-block:: sh

   shell$ mpirun -n 2 valgrind --suppressions=$PREFIX/share/openmpi/openmpi-valgrind.supp

More information on suppression-files and how to generate them can be
found in `Valgrind's documentation
<https://valgrind.org/docs/manual/manual-core.html#manual-core.suppress>`_.

/////////////////////////////////////////////////////////////////////////

How to use MPIR-based tools with Open MPI
---------------------------------------------------------------

A shim module is available for use with debuggers and tools which rely on the
`MPIR specification <https://www.mpi-forum.org/docs/mpir-specification-03-01-2018.pdf>`_
to access application process mappings.

The source code for this shim module can be downloaded from 
`MPIR to PMIx Shim repository <https://github.com/openpmix/mpir-to-pmix-guide>`_.

Instructions for use of the shim module are available at
`<https://github.com/openpmix/mpir-to-pmix-guide/blob/master/README.md>`_.

/////////////////////////////////////////////////////////////////////////

Verification of MPIR shim functionality using Open MPI
---------------------------------------------------------------

Correct operation of the MPIR shim layer with Open MPI can be verified by
following these instructions.

* Build a current version of Open MPI and install it into a sub-directory of the
  test directory, for instance, ``/home/shim_test/ompi``. The build should use the
  versions of PMIX and PRRTE included with Open MPI. The configure
  ``--with-pmix``, ``-with-prrte``, ``--with-pmix-libdir`` and
  ``--with-prrte-libdir`` flags should not be used.

* Clone the mpir-to-pmix-guide Github repository into the test directory by
  running ``git clone git@github.com:openpmix/mpir-to-pmix-guide.git``.

* Change the directory to the directory the MPIR shim code was cloned into,
  for instance ``/home/shim_test/mpir-to-pmix-guide``.

* Generate the configuration files by running ``./autogen.sh``.

* Run the configure step, specifying the test shim install directory and the
  location of the PMIX runtime. For instance
  ``./configure --prefix=/home/shim_test --with-pmix=/home/shim_test/ompi
  --with-pmix-libdir=/home/shim_test/ompi/lib``.

* Run the ``make`` and ``make install`` commands.

* Change the directory back to the top-level test directory (``/home/shim_test``).

* Set the following environment variables:

.. code-block:: sh

  export MPI_ROOT=/home/shim_test/ompi
  export PATH=/home/shim_test/bin:$MPI_ROOT/bin:$PATH
  export LD_LIBRARY_PATH=/home/shim_test/lib:$MPI_ROOT/lib:$LD_LIBRARY_PATH

* Change the directory to the test directory where the MPIR shim build created
  the test driver program.  For instance ``/home/shim_test/mpir-to-pmix-guide/test``.

* Create the following test program, ``testprog.c``. The duration of the sleep can
  be modified to whatever value suspends the test target program long enough
  for you to attach to running mpirun processes:

.. code-block:: sh

  #include <mpi.h>
  #include <unistd.h>

  void main(int argc, char **argv) {
    MPI_Init(&argc, &argv);
    sleep(60);
    MPI_Finalize();
  }

* Compile the target test program, for instance ``mpicc -o testprog testprog.c``

* Run the shim test program in Proxy Mode,
  ``mpirshim_test mpirun -n 2 ./testprog``.

* The test program should display output including one line for each application
  task showing task rank, execution node, executable path, and application task
  PID.

* To run the test program in Attach Mode, first invoke ``mpirun`` as
  ``mpirun -n 2 ./testprog`` and note the PID displayed, which is the PID of
  the ``mpirun`` process.

* Then run the shim test program as ``mpirshim_test -c <pid>`` where ``<pid>`` is
  the PID of the ``mpirun`` program.

* The test program should display output including one line for each application
  task showing task rank, execution node, executable path, and application task
  PID.

* Note that if you modify the test program to write output to ``stdout`` or ``stderr``,
  that output will not be displayed when the program is run in Proxy mode
  since the ``mpirshim_test`` program does not forward application ``stdio`` output.
