Launching with Slurm
====================

Open MPI supports two modes of launching parallel MPI jobs under
Slurm:

#. Using Open MPI's full-features ``mpirun`` launcher.
#. Using Slurm's "direct launch" capability.

Unless there is a strong reason to use ``srun`` for direct launch, the
Open MPI team recommends using ``mpirun`` for launching under Slurm jobs.

.. note:: In versions of Open MPI prior to 5.0.x, using ``srun`` for
   direct launch could be faster than using ``mpirun``.  **This is no
   longer true.**

Using ``mpirun``
----------------

When ``mpirun`` is launched in a Slurm job, ``mpirun`` will
automatically utilize the Slurm infrastructure for launching and
controlling the individual MPI processes.
Hence, it is unnecessary to specify the ``--hostfile``,
``--host``, or ``-n`` options to ``mpirun``.

.. note:: Using ``mpirun`` is the recommended method for launching Open
   MPI jobs in Slurm jobs.

   ``mpirun``'s Slurm support should always be available, regardless
   of how Open MPI or Slurm was installed.

For example:

.. code-block:: sh

   # Allocate a Slurm job with 4 slots
   shell$ salloc -n 4
   salloc: Granted job allocation 1234

   # Now run an Open MPI job on all the slots allocated by Slurm
   shell$ mpirun mpi-hello-world

This will run the 4 MPI processes on the node(s) that were allocated
by Slurm.

Or, if submitting a script:

.. code-block:: sh

   shell$ cat my_script.sh
   #!/bin/sh
   mpirun mpi-hello-world
   shell$ sbatch -n 4 my_script.sh
   srun: jobid 1235 submitted
   shell$

Similar to the ``salloc`` case, no command line options specifying
number of MPI processes were necessary, since Open MPI will obtain
that information directly from Slurm at run time.

Using Slurm's "direct launch" functionality
-------------------------------------------

Assuming that Slurm was configured with its PMIx plugin, you can use
``srun`` to "direct launch" Open MPI applications without the use of
Open MPI's ``mpirun`` command.

First, you must ensure that Slurm was built and installed with PMIx
support.  This can determined as shown below:

.. code-block:: sh

   shell$ srun --mpi=list
   MPI plugin types are...
	none
	pmi2
	pmix
   specific pmix plugin versions available: pmix_v4

The output from ``srun`` may vary somewhat depending on the version of Slurm installed.
If PMIx is not present in the output, then you will not be able to use srun
to launch Open MPI applications.

.. note:: PMI-2 is not supported in Open MPI 5.0.0 and later releases.

Provided the Slurm installation includes the PMIx plugin, Open MPI applications 
can then be launched directly via the ``srun`` command.  For example:

.. code-block:: sh

   shell$ srun -N 4 --mpi=pmix mpi-hello-world

Or you can use ``sbatch`` with a script:

.. code-block:: sh

   shell$ cat my_script.sh
   #!/bin/sh
   srun --mpi=pmix mpi-hello-world
   shell$ sbatch -N 4 my_script.sh
   srun: jobid 1235 submitted
   shell$

Similar using ``mpirun`` inside of an ``sbatch`` batch script, no
``srun`` command line options specifying number of processes were
necessary, because ``sbatch`` set all the relevant Slurm-level
parameters about number of processes, cores, partition, etc.

Slurm 20.11
-----------

There were some changes in Slurm behavior that were introduced in
Slurm 20.11.0 and subsequently reverted out in Slurm 20.11.3.

SchedMD (the makers of Slurm) strongly suggest that all Open MPI users
avoid using Slurm versions 20.11.0 through 20.11.2.

Indeed, you will likely run into problems using just about any version
of Open MPI these problematic Slurm releases.

.. important:: Please either downgrade to an older version or upgrade
               to a newer version of Slurm.
