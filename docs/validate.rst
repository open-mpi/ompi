Validating your installation
============================

Checking your Open MPI configuration
------------------------------------

The :ref:`ompi_info(1) <man1-ompi_info>` command can be used to check
the status of your Open MPI installation (located in
``$prefix/bin/ompi_info``).  Running it with no arguments provides a
summary of information about your Open MPI installation.

Note that the :ref:`ompi_info(1) <man1-ompi_info>` command is
extremely helpful in determining which components are installed as
well as listing all the run-time settable parameters that are
available in each component (as well as their default values).

The following :ref:`ompi_info(1) <man1-ompi_info>` options may be
helpful:

* ``--all``: Show a *lot* of information about your Open MPI
  installation.
* ``--parsable``: Display all the information in a machine-parsable
  format.
* ``--param FRAMEWORK COMPONENT``:
  A ``FRAMEWORK`` value of ``all`` and a ``COMPONENT`` value of ``all`` will
  show all parameters to all components.  Otherwise, the parameters of
  all the components in a specific framework, or just the parameters
  of a specific component can be displayed by using an appropriate
  FRAMEWORK and/or COMPONENT name.
* ``--level LEVEL``:
  By default, ``ompi_info`` only shows "Level 1" MCA parameters |mdash|
  parameters that can affect whether MPI processes can run
  successfully or not (e.g., determining which network interfaces to
  use).  The ``--level`` option will display all MCA parameters from
  level 1 to ``LEVEL`` (the max ``LEVEL`` value is 9).  Use ``ompi_info
  --param FRAMEWORK COMPONENT --level 9`` to see *all* MCA parameters
  for a given component.  See "The Modular Component Architecture
  (MCA)" section, below, for a fuller explanation.

Changing the values of these MCA parameters is explained in the
:ref:`Tuning section <label-running-setting-mca-param-values>`.


Testing your Open MPI installation
----------------------------------

When verifying a new Open MPI installation, we recommend running the
following tests in order (the tests build upon each other):

#. Use :ref:`mpirun(1) <man1-mpirun>` to launch a non-MPI program
   (e.g., ``hostname`` or ``uptime``) across multiple nodes.
#. Use :ref:`mpirun(1) <man1-mpirun>` to launch a trivial MPI program
   that does no MPI communication (e.g., the ``hello_c`` program in
   the ``examples/`` directory in the Open MPI distribution).
#. Use :ref:`mpirun(1) <man1-mpirun>` to launch a trivial MPI program
   that sends and receives a few MPI messages (e.g., the ``ring_c``
   program in the ``examples/`` directory in the Open MPI
   distribution).
#. Use :ref:`oshrun(1) <man1-oshrun>` to launch a non-OpenSHMEM
   program across multiple nodes.
#. Use :ref:`oshrun(1) <man1-oshrun>` to launch a trivial MPI program
   that does no OpenSHMEM communication (e.g., ``hello_shmem.c``
   program in the ``examples/`` directory in the Open MPI
   distribution.)
#. Use :ref:`oshrun <man1-oshrun>` to launch a trivial OpenSHMEM
   program that puts and gets a few messages (e.g., the
   ``ring_shmem.c`` in the ``examples/`` directory in the Open MPI
   distribution.)

If you can run all of these tests successfully, that is a good
indication that Open MPI built and installed properly.
