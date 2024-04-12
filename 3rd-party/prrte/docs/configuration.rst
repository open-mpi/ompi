PRRTE DVM Configuration
=======================

The PMIx Reference RunTime Environment (PRRTE) can be instantiated
as a Distributed Virtual Machine (DVM) in two ways. First, the
``prte`` command can be executed at a shell prompt. This will discover
the available resources (either from hostfile or as allocated by a
resource manager) and start a PRRTE shepherd daemon (:ref:`prted(1)
<man1-prted>`) on each of the indicated nodes.

The other method, however, is to bootstrap the DVM at time of cluster
startup. Bootstrapping PRRTE allows the DVM to serve as the system-level
runtime, providing a full-service PMIx environment to sessions under
its purview. Integration to an appropriately enabled scheduler can
provide a full workload managed environment for users.

Establishing the DVM using the bootstrap method requires that a PRRTE
configuration file be created and made available on every node of the
cluster at node startup. The configuration file provides necessary
information for establishing the communication infrastructure between
the DVM controller and the compute node daemons. It also provides a
means for easily defining DVM behavior for options such as logging,
system-level prolog and epilog scripts for each session, and other
PRRTE features.

The configuration file can be manually created or can be created using
the `PRRTE configuration tool <configurator.html>`_.
Manual creation can best be done
by editing the example configuration file (``<source-location>/src/etc/prte.conf``).
This file contains all the supported configuration options, with all
entries commented out. Simply uncomment the options of interest and
set them to appropriate values. The file will be installed into the
final ``<install-location>/etc`` when ``make install`` is performed.

Configuration Options
---------------------

The following options are supported by PRRTE |prte_ver|.
While we make every effort to maintain compatibility with prior versions,
we recommend that you check options when installing new versions to
see what may have changed and/or been added. We also recommend that
you use the `PRRTE DVM configurator <configurator.html>`_ for the
version you are using to ensure that it is fully compatible.

Bootstrap Options
^^^^^^^^^^^^^^^^^
``ClusterName=<string> (default: "cluster")`` is the name of the cluster upon
which the DVM is executing. This is used by PRRTE to form the namespace
for the DVM daemons, which is taken as ``<clustername>-prte-dvm``.
Using different names for each of your clusters is important if you use a single
database to record information from multiple PRRTE-managed clusters.

``DVMControllerHost=<hostname>`` is the host upon which the DVM controller
will be executing. The ``prted`` that finds itself booting onto this host
will declare itself to be the system controller and will initialize itself
accordingly.

``DVMControllerPort=<number> (default: 7817)`` is the TCP port upon which the
DVM controller will be listening for connections from its ``prted`` daemons
on the remaining system nodes.

``PRTEDPort=<number (default: 7818)`` is the TCP port upon which each
``prted`` daemon will be listening for connections from its peer daemons
on the other system nodes.

``DVMNodes=<regex of DVM nodes> (default: none)`` provides a regular expression
identifying the nodes that upon which user applications can run. IP addresses can
be provided in place of hostnames if desired.The regular expression can consist of
a simple comma-delimited list of hostnames, or a comma-delimited list of hostname
ranges (e.g., "linux0,linux[2-10]"), or a PMIx "native" regular expression.


Operational Options
^^^^^^^^^^^^^^^^^^^
``DVMTempDir=<path> (default: /tmp)`` is the temporary directory that the
DVM daemons and controller are to use as the base for their session directories.
Working files/directories for the DVM will be placed under this location.

``SessionTmpDir=<path> (default: DVMTempDir)`` is the temporary directory that
the DVM daemons are to use as the base for session directories for all
application sessions. Working files for each session will be placed under
this location, separated out into a directory for each session.

Logging Options
^^^^^^^^^^^^^^^
``ControllerLogJobState=<true|false> (default: false)`` directs the DVM
controller to log each DVM-launched job state transition. Log entry includes
the namespace of the job, the state to which it is transitioning, and the
date/time stamp when the transition was ordered.

``ControllerLogProcState=<true|false> (default: false)`` directs the DVM
controller to log each process (in a DVM-launched job) state transition.
Log entry includes the namespace and rank of the process, the state to
which it is transitioning, and the date/time stamp when the transition was
ordered.

``ControllerLogPath=<path> (default: DVMTempDir)`` is the path to where the logs are to
be written. If a relative path is provided,
then the directory will be created under the ``DVMTempDir`` location. The
path defaults to the specified SessionTmpDir in the absence of any input
to this field. The log filename is formatted as ``prtectrlr-<hostname>-log<``.

``PRTEDLogJobState=<true|false> (default: false)`` directs each ``prted``
in the DVM to log each DVM-launched job state transition. Log entry includes
the namespace of the job, the state to which it is transitioning, and the
date/time stamp when the transition was ordered.

``PRTEDLogProcState=<true|false> (default: false)`` directs each ``prted``
in the DVM to log each process (in a DVM-launched job) state transition.
Log entry includes the namespace and rank of the process, the state to
which it is transitioning, and the date/time stamp when the transition was
ordered.

``PRRTEDLogPath=<path> (default: DVMTempDir)`` is the path to where the logs are to
be written. If a relative path is provided,
then the directory will be created under the ``DVMTempDir`` location. The
path defaults to the specified SessionTmpDir in the absence of any input
to this field. The log filename is formatted as ``prted-<hostname>-log<``.


Configurator Tool
-----------------

The `PRRTE configuration tool <configurator.html>`_ contains all the supported options in an
easy-to-use form. Once you have filled out the desired entries, the
"submit" button will show the resulting configuration file on the
browser window |mdash| a simple "copy/paste" operation into your target
configuration file will yield the final result.
