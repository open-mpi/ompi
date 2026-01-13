Launching with HPE PALS
=======================

Open MPI supports two modes of launching parallel MPI jobs on HPE
systems with HPE PALS (Parallel Application Launch Service) installed and enabled:

#. Using Open MPI's full-featured ``mpirun`` launcher.
#. Using the PALS "direct launch" capability with the ``aprun`` launcher.

Unless there is a strong reason to use ``aprun`` for direct launch, the
Open MPI team recommends using ``mpirun`` for launching jobs on these
systems.

PALS is available on HPE systems which uses PBS/Torque for the
resource manager.  It supports a PMIx server, albeit with some limitations
compared to recent PRRTE releases.

Information about PALS can be found at `HPE's support portal <http://support.hpe.com/>`_.  Search for
**parallel application launch service.**

.. note:: Open MPI has only been tested against PALS 1.5.0. PALS support was introduced
   in PRRTE starting with release 4.0.0.

Since PALS is currently only available on HPE systems managed with PBS, also see the **Launching with PBS/Torque**
documentation :doc:`tm`.

Verify PALS support
-------------------

The ``prte_info`` command can be used to determine whether or not an
installed Open MPI includes PALS support:

.. code-block::

   shell$ prte_info | grep pals

If the Open MPI installation includes support for PALS, you
should see lines similar to those below. Note the MCA version
information varies depending on which version of PRRTE is
installed.

.. code-block::

       MCA ess: pals (MCA v2.1.0, API v3.0.0, Component v5.0.0)
       MCA plm: pals (MCA v2.1.0, API v2.0.0, Component v5.0.0)

Using ``mpirun``
----------------

This section assumes there is PALS support in the PRRTE being used for the Open MPI installation.

When ``mpirun`` is launched in a PBS job, ``mpirun`` will
automatically utilize the PALS infrastructure for launching and
controlling the individual MPI processes.

.. note:: Using ``mpirun`` is the recommended method for launching Open
   MPI jobs on HPE systems where PALS is available. This is primarily due to limitations in the
   PMIx server provided in PALS.

For example:

.. code-block:: sh

   # Allocate a PBS job with 32 slots on 1 node
   shell$ qsub -I -l select=1:ncpus=32:mpiprocs=32,filesystems=home -lwalltime=0:30:00 -lwalltime=10:00 -Afoobar
   qsub: waiting for job XXX to start
   qsub: job XXX ready

   # Now run an Open MPI job on all the slots allocated by PBS
   shell$ mpirun mpi-hello-world

This will run the 32 MPI processes on the node that was allocated by PBS.

Using PALS "direct launch" functionality
----------------------------------------

The HPE PALS 1.5.0 documentation states that it comes pre-built with PMIx support.
By default the PALS ``aprun`` launcher does not use PMIx.  To use the launcher's
PMIx capabilities either the command line option ``--pmix=pmix`` needs to be set
or the ``ALPS_PMI`` environment variable needs to be set to ``pmix``.

.. code-block:: sh

   shell$ aprun -n 4 -N 2 --pmi=pmix mpi-hello-world

   or

   shell$ ALPS_PMI=pmix aprun -n 4 -N 2  mpi-hello-world

In these examples, four instances of the application are started, two instances per node.

See the PALS ``aprun`` man page for documentation on how to this command.
