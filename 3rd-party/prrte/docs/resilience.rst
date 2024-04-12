Resilience
==========

This section documents the features and options specific to the **PRTE
Level Fault Tolerant** PMIx reference RunTime Environment (PRTE)

Features
--------

This implementation provides a runtime level failure detection and
propagation mechanism for both process and node failure.

What's added to support fault-tolerance?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. New module under src/mca/errmgr: detector:
   Daemons monitor one another along a ring topology to detect node failures.
   src/mca/odls is in charge of detecting the failure of locally hosted processes
   (using SIGCHLD signals from the operating system).

#. New component: propagate with module prperror:
   Prepares the content of the reliable broadcast messages (i.e., the list of failed processes).
   In order to populate the list of failed processes in node failure cases, the list of processes
   hosted by a particular daemon is collected by prperror module.

#. New module under src/mca/grpcomm: bmg
   The BMG component implements a broadcast algorithm in a reliable way;
   to be noted, this component abides by the normal interface for a daemon
   broadcast and can reliably broadcast any type of information

#. Test case for process failure under example/error_notify.c
   This test uses kill(pid) to kill a process to simulate process failure.

#. Test case for daemon/node failure under example/daemon_error_notify.c
   This test uses kill(ppid) to kill a process's parent to simulate node failure.

Building
^^^^^^^^

.. code-block:: bash

   ./autogen.pl

   # If you want to run mpi applications, you mpi and PRTE
   # should have the same version of PMIx and libevent

   ./configure --enable-prte-ft --prefix=...  --with-pmix=/external-pmix-path --with-libevent=/external-libevent-path

   make [-j N] all install
   #    use an integer value of N for parallel builds

Running
^^^^^^^

Building your application
+++++++++++++++++++++++++

Compile your application as usual

#. using the provided ``pcc`` for pmix-based application;
#. using your ``mpicc`` for mpi-based application with a prte-based MPI (e.g., Open MPI).

Running your application
++++++++++++++++++++++++

If running standalone:

#. you need to launch first the DVM daemons with ``prte --mca prte_enable_ft true``.
#. You can then launch your application with by simply using the provided ``prun --enable-recovery``.

Make sure to set your ``PATH`` and ``LD_LIBRARY_PATH`` properly.

If running with a PRRTE-based MPI (e.g., Open MPI):

#. use ``mpiexec --enable-recovery --mca prte_enable_ft true``.

Running under a batch scheduler
+++++++++++++++++++++++++++++++

This code can operate under a job/batch scheduler, and is tested routinely with Slurm.
One difficulty comes from the fact that many job schedulers will "cleanup" the
application as soon as a process fails. In order to avoid this problem, it is preferred
that you use ``-k, --no-kill [=off]: Do not automatically terminate a job if one of the nodes
it has been allocated fails.`` within an allocation (e.g., ``salloc``, ``sbatch``) rather than
a direct launch (e.g. ``srun``).

Run-time tuning knobs
^^^^^^^^^^^^^^^^^^^^^

This code comes with a variety of knobs for controlling how it runs. The default
parameters are sane and should result in very good performance in most
cases. You can change those default by ``--prtemca parameter value``:

* ``prte_enable_recovery <true|false> (default: false)`` controls automatic
  cleanup of apps with failed processes.

* ``prte_abort_non_zero_exit <true|false> (default: true)`` controls the job
  termination after a error occurred.

* ``errmgr_detector_enable <true|false> (default: false)`` enable or disable error
  detection and propagation.

* ``errmgr_detector_heartbeat_period <float> (default:5s)`` heartbeat
  period. Recommended value is 1/2 of the timeout.

* ``errmgr_detector_heartbeat_timeout <float> (default:10s)`` heartbeat
  timeout (i.e. failure detection speed). Recommended value is 2 times
  the heartbeat period

To be noted: if you want to use prte failure detection and propagation features.
             You MUST set prte_enable_recovery to true,
             prte_abort_non_zero_exit to false.

Testing
-------

.. code-block:: bash

   # Step 1
   salloc -k -N num_of_nodes -w host1,host2...
        -k, --no-kill do not kill job on node failure

   # Step 2
   prte --prtemca prte_enable_ft true \
        --prtemca errmgr_detector_heartbeat_period 0.5  \
        --prtemca errmgr_detector_heartbeat_timeout 1  \
        --prtemca errmgr_detector_enable 1 \
        --prtemca prte_abort_on_non_zero_status 0 \
        --debug-daemons

   # using 'errmgr_detector_enable 1' choose enable the error detector.

Config with ``--enable-debug``, ``--debug-daemons`` will give you lots of information.

Also, the ring detector heartbeat sending frequency is not hard coded,
you can change heartbeat_peroid and heartbeat_timeout by using MCA
params.  For example:

* using ``--prtemca errmgr_detector_heartbeat_period 10`` set the sending frequency to every 10 seconds(default is 5s)

* using ``--prtemca errmgr_detector_heartbeat_timeout 20`` set timeout to 20 seconds(default is 10s)

Step 3: under example we have 2 test codes ``error_notify.c``,
``daemon_error_notify.c``:

.. code-block:: bash

   # Compile the codes
   pcc -g error_notify.c -o error_notify

   # Run
   prun --oversubscribe --merge-stderr-to-stdout \
        --map-by node:DISPLAY:DISPLAYALLOC \
        --report-bindings --enable-recovery \
        --max-restarts 4 \
        --continuous -np num_of_procs error_notify -v

If use external pmix:

.. code-block:: bash

   # Compile
   pcc error_notify.c -o error_notify_1 \
        -I/external_pmix_install_path/include \
        -L/external_pmix_install_path/lib \
        -lpmix

   # Run
   prun --oversubscribe -x LD_LIBRARY_PATH \
        --merge-stderr-to-stdout \
        --map-by node:DISPLAY:DISPLAYALLOC \
        --report-bindings --enable-recovery \
        --max-restarts 4 \
        --continuous -np num_of_procs error_notify_1 -v

Iif use external pmix:

.. code-block:: bash

   # Compile
   pcc daemon_error_notify.c -o daemon_error_notify_1 \
        -I/external_pmix_install_path/include \
        -L/external_pmix_install_path/lib \
        -lpmix

   # Run
   prun --oversubscribe -x LD_LIBRARY_PATH \
        --merge-stderr-to-stdout \
        --map-by node:DISPLAY:DISPLAYALLOC \
        --report-bindings --enable-recovery \
        --max-restarts 4 \
        --continuous -np num_of_procs \
        daemon_error_notify_1 -v

Copyright
---------

Copyright (c) 2018-2020 The University of Tennessee and The University
of Tennessee Research Foundation.  All rights reserved.

Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved.
Copyright (c) 2024      Nanook Consulting  All rights reserved.
$COPYRIGHT$

Additional copyrights may follow

$HEADER$
