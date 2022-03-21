Launching with Grid Engine
==========================

Open MPI supports the family of run-time schedulers including the Sun
Grid Engine (SGE), Oracle Grid Engine (OGE), Grid Engine (GE), Son of
Grid Engine, and others.

This documentation will collectively refer to all of them as "Grid
Engine", unless a referring to a specific flavor of the Grid Engine
family.

Verify Grid Engine support
--------------------------

.. important:: To build Grid Engine support in Open MPI, you will need
   to explicitly request the SGE support with the ``--with-sge``
   command line switch to Open MPI's ``configure`` script.

To verify if support for Grid Engine is configured into your Open MPI
installation, run ``prte_info`` as shown below and look for
``gridengine``.

.. code-block::

   shell$ prte_info | grep gridengine
                 MCA ras: gridengine (MCA v2.0, API v2.0, Component v1.3)

.. note:: PRRTE is the software layer that provides run-time
   environment support to Open MPI.  Open MPI typically hides most
   PMIx and PRRTE details from the end user, but this is one place
   that Open MPI is unable to hide the fact that PRRTE provides this
   functionality, not Open MPI.  Hence, users need to use the
   ``prte_info`` command to check for Grid Engine support (not
   ``ompi_info``).

Launching
---------

When Grid Engine support is included, Open MPI will automatically
detect when it is running inside SGE and will just "do the Right
Thing."

Specifically, if you execute an ``mpirun`` command in a Grid Engine
job, it will automatically use the Grid Engine mechanisms to launch
and kill processes.  There is no need to specify what nodes to run on
|mdash| Open MPI will obtain this information directly from Grid
Engine and default to a number of processes equal to the slot count
specified.  For example, this will run 4 MPI processes on the nodes
that were allocated by Grid Engine:

.. code-block:: sh

   # Get the environment variables for Grid Engine

   # (Assuming Grid Engine is installed at /opt/sge and $Grid
   # Engine_CELL is 'default' in your environment)
   shell$ . /opt/sge/default/common/settings.sh

   # Allocate an Grid Engine interactive job with 4 slots from a
   # parallel environment (PE) named 'ompi' and run a 4-process Open
   # MPI job
   shell$ qrsh -pe ompi 4 -b y mpirun -n 4 mpi-hello-world

There are also other ways to submit jobs under Grid Engine:

.. code-block:: sh

   # Submit a batch job with the 'mpirun' command embedded in a script
   shell$ qsub -pe ompi 4 my_mpirun_job.csh

   # Submit a Grid Engine and OMPI job and mpirun in one line
   shell$ qrsh -V -pe ompi 4 mpirun hostname

   # Use qstat(1) to show the status of Grid Engine jobs and queues
   shell$ qstat -f

In reference to the setup, be sure you have a Parallel Environment
(PE) defined for submitting parallel jobs. You don't have to name your
PE "ompi".  The following example shows a PE named "ompi" that would
look like:

.. code-block::

   shell$ qconf -sp ompi
      pe_name            ompi
      slots              99999
      user_lists         NONE
      xuser_lists        NONE
      start_proc_args    NONE
      stop_proc_args     NONE
      allocation_rule    $fill_up
      control_slaves     TRUE
      job_is_first_task  FALSE
      urgency_slots      min
      accounting_summary FALSE
      qsort_args         NONE

.. note:: ``qsort_args`` is necessary with the Son of Grid Engine
   distribution, version 8.1.1 and later, and probably only applicable
   to it.

.. note:: For very old versions of Sun Grid Engine, omit
   ``accounting_summary`` too.

You may want to alter other parameters, but the important one is
``control_slaves``, specifying that the environment has "tight
integration".  Note also the lack of a start or stop procedure.  The
tight integration means that mpirun automatically picks up the slot
count to use as a default in place of the ``-n`` argument, picks up a
host file, spawns remote processes via ``qrsh`` so that Grid Engine
can control and monitor them, and creates and destroys a per-job
temporary directory (``$TMPDIR``), in which Open MPI's directory will
be created (by default).

Be sure the queue will make use of the PE that you specified:

.. code-block::

   shell$ qconf -sq all.q
   [...snipped...]
   pe_list               make cre ompi
   [...snipped...]

To determine whether the Grid Engine parallel job is successfully
launched to the remote nodes, you can pass in the MCA parameter
``--mca plm_base_verbose 1`` to ``mpirun``.

This will add in a ``-verbose`` flag to the ``qrsh -inherit`` command
that is used to send parallel tasks to the remote Grid Engine
execution hosts. It will show whether the connections to the remote
hosts are established successfully or not.

.. error:: TODO is this site still live?  Doesn't look like it..  Jeff
   emailed Dave Love on 31 Dec 2021 to ask if this is still the
   correct URL.

   Update March 2022: it doesn't look like this web site is good any
   more.  Perhaps use https://github.com/grisu48/gridengine instead...?

Various Grid Engine documentation with pointers to more is available
at `the Son of GridEngine site <http://arc.liv.ac.uk/sge/>`_, and
configuration instructions can be found at `the Son of GridEngine
configuration how-to site
<http://arc.liv.ac.uk/SGE/howto/sge-configs.html>`_.

Grid Engine tight integration support of the ``qsub -notify`` flag
------------------------------------------------------------------

If you are running SGE 6.2 Update 3 or later, then the ``-notify``
flag is supported.  If you are running earlier versions, then the
``-notify`` flag will not work and using it will cause the job to be
killed.

To use ``-notify``, one has to be careful.  First, let us review what
``-notify`` does.  Here is an excerpt from the qsub man page for the
``-notify`` flag.

  The ``-notify`` flag, when set causes Sun Grid Engine to send
  warning signals to a running job prior to sending the signals
  themselves. If a SIGSTOP is pending, the job will receive a SIGUSR1
  several seconds before the SIGSTOP.  If a SIGKILL is pending, the
  job will receive a SIGUSR2 several seconds before the SIGKILL.  The
  amount of time delay is controlled by the notify parameter in each
  queue configuration.

Let us assume the reason you want to use the ``-notify`` flag is to
get the SIGUSR1 signal prior to getting the SIGTSTP signal.  Something
like this batch script can be used:

.. code-block:: sh

   #! /bin/bash
   #$ -S /bin/bash
   #$ -V
   #$ -cwd
   #$ -N Job1
   #$ -pe ompi 16
   #$ -j y
   #$ -l h_rt=00:20:00
   mpirun -n 16 -mca orte_forward_job_control 1 mpi-hello-world

.. error:: Ralph: Does ``orte_forward_job_control`` still exist?

However, one has to make one of two changes to this script for things
to work properly.  By default, a SIGUSR1 signal will kill a shell
script.  So we have to make sure that does not happen. Here is one way
to handle it:

.. code-block:: sh

   #! /bin/bash
   #$ -S /bin/bash
   #$ -V
   #$ -cwd
   #$ -N Job1
   #$ -pe ompi 16
   #$ -j y
   #$ -l h_rt=00:20:00
   exec mpirun -n 16 -mca orte_forward_job_control 1 mpi-hello-world

Alternatively, one can catch the signals in the script instead of doing
an exec on the mpirun:

.. code-block:: sh

   #! /bin/bash
   #$ -S /bin/bash
   #$ -V
   #$ -cwd
   #$ -N Job1
   #$ -pe ompi 16
   #$ -j y
   #$ -l h_rt=00:20:00

   function sigusr1handler()
   {
       echo "SIGUSR1 caught by shell script" 1>&2
   }

   function sigusr2handler()
   {
       echo "SIGUSR2 caught by shell script" 1>&2
   }

   trap sigusr1handler SIGUSR1
   trap sigusr2handler SIGUSR2

   mpirun -n 16 -mca orte_forward_job_control 1 mpi-hello-world

Grid Engine job suspend / resume support
----------------------------------------

To suspend the job, you send a SIGTSTP (not SIGSTOP) signal to
``mpirun``.  ``mpirun`` will catch this signal and forward it to the
``mpi-hello-world`` as a SIGSTOP signal.  To resume the job, you send
a SIGCONT signal to ``mpirun`` which will be caught and forwarded to
the ``mpi-hello-world``.

By default, this feature is not enabled.  This means that both the
SIGTSTP and SIGCONT signals will simply be consumed by the ``mpirun``
process.  To have them forwarded, you have to run the job with ``--mca
orte_forward_job_control 1``.  Here is an example on Solaris:

.. error:: TODO Ralph: does ``orte_forward_job_control`` still exist?

.. code-block:: sh

   shell$ mpirun -mca orte_forward_job_control 1 -n 2 mpi-hello-world

In another window, we suspend and continue the job:

.. code-block:: sh

   shell$ prstat -p 15301,15303,15305
      PID USERNAME  SIZE   RSS STATE  PRI NICE      TIME  CPU PROCESS/NLWP
    15305 rolfv     158M   22M cpu1     0    0   0:00:21 5.9% mpi-hello-world/1
    15303 rolfv     158M   22M cpu2     0    0   0:00:21 5.9% mpi-hello-world/1
    15301 rolfv    8128K 5144K sleep   59    0   0:00:00 0.0% mpirun/1

   shell$ kill -TSTP 15301
   shell$ prstat -p 15301,15303,15305
      PID USERNAME  SIZE   RSS STATE  PRI NICE      TIME  CPU PROCESS/NLWP
    15303 rolfv     158M   22M stop    30    0   0:01:44  21% mpi-hello-world/1
    15305 rolfv     158M   22M stop    20    0   0:01:44  21% mpi-hello-world/1
    15301 rolfv    8128K 5144K sleep   59    0   0:00:00 0.0% mpirun/1

   shell$ kill -CONT 15301
   shell$ prstat -p 15301,15303,15305
      PID USERNAME  SIZE   RSS STATE  PRI NICE      TIME  CPU PROCESS/NLWP
    15305 rolfv     158M   22M cpu1     0    0   0:02:06  17% mpi-hello-world/1
    15303 rolfv     158M   22M cpu3     0    0   0:02:06  17% mpi-hello-world/1
    15301 rolfv    8128K 5144K sleep   59    0   0:00:00 0.0% mpirun/1

Note that all this does is stop the ``mpi-hello-world`` processes.  It
does not, for example, free any pinned memory when the job is in the
suspended state.

To get this to work under the Grid Engine environment, you have to
change the ``suspend_method`` entry in the queue.  It has to be set to
SIGTSTP.  Here is an example of what a queue should look like.

.. code-block:: sh

   shell$ qconf -sq all.q
   qname                 all.q
   [...snipped...]
   starter_method        NONE
   suspend_method        SIGTSTP
   resume_method         NONE

Note that if you need to suspend other types of jobs with SIGSTOP
(instead of SIGTSTP) in this queue then you need to provide a script
that can implement the correct signals for each job type.
