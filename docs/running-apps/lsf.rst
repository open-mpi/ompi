Launching with LSF
==================

Open MPI supports the LSF resource manager.

Verify LSF support
------------------

The ``prte_info`` command can be used to determine whether or not an
installed Open MPI includes LSF support:

.. code-block::

   shell$ prte_info | grep lsf

If the Open MPI installation includes support for LSF, you
should see a line similar to that below. Note the MCA version
information varies depending on which version of Open MPI is
installed.

.. code-block::

       MCA ras: lsf (MCA v2.1.0, API v2.0.0, Component v3.0.0)

.. note:: PRRTE is the software layer that provides run-time
   environment support to Open MPI.  Open MPI typically hides most
   PMIx and PRRTE details from the end user, but this is one place
   that Open MPI is unable to hide the fact that PRRTE provides this
   functionality, not Open MPI.  Hence, users need to use the
   ``prte_info`` command to check for LSF support (not
   ``ompi_info``).

Launching
---------

When properly configured, Open MPI obtains both the list of hosts and
how many processes to start on each host from LSF directly.  Hence, it
is unnecessary to specify the ``--hostfile``, ``--host``, or ``-n``
options to ``mpirun``.  Open MPI will use LSF-native mechanisms
to launch and kill processes (``ssh`` is not required).

For example:

.. code-block:: sh

   # Allocate a job with 4 nodes and run the job on LSF allocated nodes
   shell$ bsub -n 4 "mpirun mpi-hello-world"


This will run the MPI processes on the nodes that were allocated by
LSF.  Or, if submitting a script:

.. code-block:: sh

   shell$ cat my_script.sh
   #!/bin/sh
   mpirun mpi-hello-world
   shell$ bsub -n 4 < my_script.sh
