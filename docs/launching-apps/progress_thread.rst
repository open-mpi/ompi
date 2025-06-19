.. _async-progress-thread-label:

Asynchronous progress thread
============================

OpenMPI provides an experimental support of software-based asynchronous
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

When OpenMPI was built with ``--enable-progress-threads``, the progress
thread is still deactivated at runtime by default.

The progress thread can be activated by setting one of the following
environment variables to any non-zero value in the launching command:

.. code-block:: sh

   shell$ OMPI_ASYNC_PROGRESS=1 mpirun ...
   shell$ OPAL_ASYNC_PROGRESS=7 mpirun ...

When both variables are set, ``OMPI_ASYNC_PROGRESS`` will be preferred to
``OPAL_ASYNC_PROGRESS``.

.. warning:: Activating progress thread at runtime may bring more harm
             than improvement to performance. User should try it with caution.

Rationale
---------

A possible beneficial usecase of software progress thread is *intra-node
shared-mem non-blocking* communication, running on some high core-count CPUs,
on which application may not use all the available cores, or the CPU has some
reserved cores dedicated to communication tasks. In such configuration, the
latency of some non-blocking collective operations (e.g. ``MPI_Ireduce()``)
can be improved thanks to arithmetic operations being performed in the
background by the progress thread, instead of the main thread which would only
perform computation lastly at the moment of ``MPI_Wait()``, as currently
implemented in OpenMPI.

On another hand, on systems where *inter-node communication* was already
offloaded to an appropriate BTL backend and associated hardware devices,
enabling the software-based progress thread could be harmful to performance,
since the additional thread will take up more CPU time for mostly no utility.

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
