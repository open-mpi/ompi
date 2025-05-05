Launching only on the local node
================================

It is common to develop MPI applications on a single workstation or
laptop, and then move to a larger parallel / HPC environment once the
MPI application is ready.

Open MPI supports running multi-process MPI jobs on a single machine.
In such cases, you can simply avoid listing a hostfile or remote
hosts, and simply list a number of MPI processes to launch.  For
example:

.. code-block:: sh

   shell$ mpirun -n 6 mpi-hello-world
   Hello world, I am 0 of 6 (running on my-laptop))
   Hello world, I am 1 of 6 (running on my-laptop)
   ...
   Hello world, I am 5 of 6 (running on my-laptop)

If you do not specify the ``-n`` option, ``mpirun`` will default to
launching as many MPI processes as there are processor cores (not
hyperthreads) on the machine.

MPI communication
-----------------

When running on a single machine, Open MPI will most likely use the
``ob1`` PML and the following BTLs for MPI communication between
peers:

* ``self``: used for sending and receiving loopback MPI messages
  |mdash| where the source and destination MPI process are the same.
* ``sm``: used for sending and receiving MPI messages where the source
  and destination MPI processes can share memory (e.g., via SYSV or
  POSIX shared memory mechanisms).

  .. note:: For more information about using shared memory MPI
            communication, see the :doc:`Shared Memory
            </tuning-apps/networking/shared-memory>` page.
