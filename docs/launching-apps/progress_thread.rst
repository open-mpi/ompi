.. _async-progress-thread-label:

Asynchronous progress thread
============================

Open MPI provides an experimental support of software-based asynchronous
progress thread. This progress thread is in charge of running internal
progression engine in the background to advance non-blocking overlapping
communication.

Enabling progress thread at configuration time
----------------------------------------------

The feature is can be enabled or disabled at configuration by passing
``--enable-progress-threads`` or ``--disable-progress-threads`` to
``configure``. The default state is enabled.

Enabling progress thread at runtime
-----------------------------------

When Open MPI was configured and built with ``--enable-progress-threads``, the
progress thread is still deactivated at runtime by default.

The progress thread can be activated by setting one of the following
MCA boolean variables in the launching command:

.. code-block:: sh

   shell$ mpirun --mca opal_async_progress 1 ...
   shell$ mpirun --mca mpi_async_progress 1 ...
   shell$ OMPI_MCA_opal_async_progress=1 mpirun ...
   shell$ OMPI_MCA_mpi_async_progress=1 mpirun ...

Note that ``mpi_async_progress`` is a synonym of ``opal_async_progress``.

.. warning:: Progress threads are a somewhat complicated issue. Activating them
             at run time may improve overlap of communication and computation in
             your application (particularly those with non-blocking communication)
             which will improve overall performance. But there may be unintended
             consequences which may degrade overall application performance.
             Users are advised to experiment and see what works best for their
             applications.

Rationale
---------

A possible beneficial usecase of software progress thread is *intra-node
shared-memory non-blocking* communication, running on some high core-count CPUs,
on which application may not use all the available cores, or the CPU has some
reserved cores dedicated to communication tasks. In such configurations, the
latency of some non-blocking collective operations (e.g. ``MPI_Ireduce()``)
can be improved thanks to arithmetic operations being performed in the
background by the progress thread, instead of deferring the computations to
being executed by the main thread during ``MPI_Wait()``.

Alternatively, on systems where *inter-node communications* are already
offloaded to dedicated hardware, enabling the software-based progress threads
could degrade performance, since the additional thread will force progress up
through the CPU and potentially away from more optimized hardware functionality.

For these performance reasons, the progress thread is not activated (spawned)
by default at runtime. It is upon developers to decide to switch on the
progress thread, depending on their application and system setup.

Limitations
-----------

#. The current implementation does not support (yet) binding the progress
   thread to a specific core (or set of cores).

#. There are still some hard-coded constant parameters in the code that
   would require further tuning.

#. It was observed that some multi-threading overhead may impact performance
   on small buffers.
