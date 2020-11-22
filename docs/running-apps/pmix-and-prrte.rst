The role of PMIx and PRRTE
==========================

Open MPI uses two external packages for its run-time system support:
`PMIx <https://pmix.org/>`_ and `PRRTE
<https://github.com/openpmix/prrte>`_.

.. note:: Both of these packages were originally developed as an
   internal part of Open MPI.  Over time, they split off into
   independent packages so that they could be used outside of an Open
   MPI-specific environment.

Both PMIx and PRRTE have many configure- and run-time options.  Open
MPI attempts to hide most of these details from the end user, and
instead present a unified "everything is Open MPI" interface.  Open
MPI will translate configuration directives to PMIx and PRRTE as
relevant, hiding such minutia from the end-user.

This is an intentional design decision on the part of the Open MPI
developer community: HPC and MPI are complicated enough.  We do not
want to burden the average end user with needing to understand which
abstractions and configuration options belong to Open MPI vs. PMIx
vs. PRRTE.

Advanced users can peek into the PMIx and PRRTE internals and tweak
additional configuration settings if necessary, but we hope that that
will rarely be necessary.

PMIx
----

The `Process Management Interface for Exascale (PMIx)
<https://pmix.org>`_ package is used by Open MPI for the management
communication and coordination of MPI processes with a back-end
run-time system.

The "back-end run-time system" may range from a low-infrastructure
system that simply uses ``ssh`` to remotely execute commands (with no
other infrastructure) to an environment with a full-featured resource
manager and scheduler such as Slurm, PBS/Pro/Torque, or LSF.

PMIx presents a unified API that hides many of the complexities of
communication with these back-end run-time environments.  Open MPI
uses the PMIx API to discover, communicate, and coordinate with any
supported back-end run-time system without needing to know the
intimiate details of that system.

PRRTE
-----

The `PMIx Reference Runtime Environment
<https://github.com/openpmix/prrte>`_ is, as its name implies, a
reference run-time environment that utilizes the PMIx API.  It mainly
provides run-time environment infrastructure for environments that do
not natively have them.  In practical terms, this typically means
providing infrastructure for non-scheduled environments that have no
concept of distributed scheduling, file staging, remote stdout/stderr
redirection, and only have ``ssh`` to execute commands on remote
nodes.

Open MPI uses PRRTE to deal with the practical issues of the back-end
run-time environment such as launching, monitoring, killing, and
reaping remote processes.
