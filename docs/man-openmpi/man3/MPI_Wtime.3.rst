.. _mpi_wtime:


MPI_Wtime
=========

.. include_body

:ref:`MPI_Wtime` |mdash| Returns an elapsed time on the calling processor.


SYNTAX
------


C Syntax
^^^^^^^^

.. code-block:: c

   #include <mpi.h>

   double MPI_Wtime()


Fortran Syntax
^^^^^^^^^^^^^^

.. code-block:: fortran

   USE MPI
   ! or the older form: INCLUDE 'mpif.h'
   DOUBLE PRECISION MPI_WTIME()


Fortran 2008 Syntax
^^^^^^^^^^^^^^^^^^^

.. code-block:: fortran

   USE mpi_f08
   DOUBLE PRECISION MPI_WTIME()


RETURN VALUE
------------

Time in seconds since an arbitrary time in the past.


DESCRIPTION
-----------

:ref:`MPI_Wtime` returns a floating-point number of seconds, representing
elapsed wall-clock time since some time in the past.

The "time in the past" is guaranteed not to change during the life of
the process. The user is responsible for converting large numbers of
seconds to other units if they are preferred.

This function is portable (it returns seconds, not "ticks"), it allows
high resolution, and carries no unnecessary baggage. One would use it
like this:

.. code-block:: c

       {
          double starttime, endtime;
          starttime = MPI_Wtime();
           ....  stuff to be timed  ...
          endtime   = MPI_Wtime();
          printf("That took %f seconds\n",endtime-starttime);
       }

The times returned are local to the node that called them. There is no
requirement that different nodes return the "same" time.


NOTES
-----

The boolean variable MPI_WTIME_IS_GLOBAL, a predefined attribute key
that indicates whether clocks are synchronized, does not have a valid
value in Open MPI, as the clocks are not guaranteed to be synchronized.

This function is intended to be a high-resolution, elapsed (or wall)
clock. See :ref:`MPI_Wtick` to determine the resolution of :ref:`MPI_Wtime`.

On POSIX platforms, this function may utilize a timer that is cheaper
to invoke than the gettimeofday() system call, but will fall back to
gettimeofday() if a cheap high-resolution timer is not available. The
:ref:`ompi_info(1) <man1-ompi_info>` command can be consulted to see if
Open MPI supports a native high-resolution timer on your platform; see
the value for ":ref:`MPI_WTIME` support" (or "options:mpi-wtime" when
viewing the parsable output). If this value is "native", a method that
is likely to be cheaper than gettimeofday() will be used to obtain the
time when :ref:`MPI_Wtime` is invoked.

For example, on platforms that support it, the *clock_gettime()*
function will be used to obtain a monotonic clock value with whatever
precision is supported on that platform (e.g., nanoseconds).

Note, too, that the MCA parameter opal_timer_require_monotonic can
influcence this behavior. It defaults to true, but if set to false, Open
MPI may use a finer-grained timing mechanism (e.g., the RDTSC/RDTSCP
clock ticks on x86_64 platforms), but is not guaranteed to be monotonic
in some cases (e.g., if the MPI process is not bound to a single
processor core).

This function does not return an error value. Consequently, the result
of calling it before :ref:`MPI_Init` or after :ref:`MPI_Finalize` is undefined.


.. seealso::
   * :ref:`MPI_Wtick`
