.. _man1-palloc:

palloc
=========

.. include_body

palloc |mdash| Request a resource allocation

SYNOPSIS
--------

``palloc [options]``


DESCRIPTION
-----------

``palloc`` allows a user to request an allocation of resources,
either as part of a job script or for an interactive session.
In addition to describing the requested resources, users can
provide a job script or application to be executed upon establishment
of the resulting session.

Note that ``palloc`` can also be used to *modify* an existing
allocation or allocation request - e.g., extending its time limit,
or changing the allocated resources.


OPTIONS
-------

``palloc`` accepts the following options:

* ``-h`` | ``--help <arg0>``: Show help message. If the optional
  argument is not provided, then a generalized help message similar
  to the information provided here is returned. If an argument is
  provided, then a more detailed help message for that specific
  command line option is returned.

* ``-v`` | ``--verbose``: Enable debug output.

* ``-V`` | ``--version``: Print version and exit.

* ``--uri <arg0>``: Specify the URI of the server to which we are to connect, or
  the name of the file (specified as ``file:filename``) that contains that info

* ``--namespace <arg0>``: Namespace of the daemon to which we should connect

* ``--nspace <arg0>``: Synonym for ``namespace``

* ``--pid <arg0>``: PID of the daemon to which we should connect (``int`` => ``PID``
  or ``file:<file>`` for file containing the PID

* ``--system-server-first``: First look for a system server and connect to it if found

* ``--system-server-only``: Connect only to a system-level server

* ``--system-controller``: Connect to the system controller

* ``--tmpdir <arg0>``: Set the root for the session directory tree

* ``--connect-order <arg0>``: Specify search order for server connections - e.g., scheduler, system controller, system-level server, or local server

* ``--wait-to-connect <arg0>``: Delay specified number of seconds before trying to connect

* ``--num-connect-retries <arg0>``: Max number of times to try to connect

* ``--request-id <arg0>``: String identifier for this allocation request

* ``-q`` | ``--queue <arg0>``: Scheduler queue this request should be passed to for processing

* ``-N`` | ``--nodes <arg0>``: Number of nodes to be allocated

* ``-i`` | ``--image <arg0>``: OS image to be provisioned on allocated nodes prior to initiating execution of specified job, or turning nodes over to an interactive session

* ``-x`` | ``--exclude <arg0>``: Comma-delimited list of nodes that are to be excluded from consideration for scheduling this allocation

* ``--wait-all-nodes``: Wait for all nodes to be ready before starting execution of the specified job.

* ``-w`` | ``--nodelist <arg0>``: Comma-delimited list of ranges of specific nodes being requested [e.g., host0[1-5],host128]. Can also pass the argument as a filename using the "file:<path>" syntax. Ordering of names and/or duplicate names are ignored.

* ``--uid <arg0>``: Assign the resulting allocation to the specified user ID

* ``--gid <arg0>``: Assign the resulting allocation to the specified group ID

* ``-t`` | ``--time <arg0>``: Time limit on the assigned allocation.

* ``--signal <arg0>[@arg1]``: Send all processes executing within the allocated session the specified signal when it reaches the (optional) specified seconds of its end time.

* ``-s`` | ``--share``: Allocated resources can be shared with other allocations.

* ``--extend   <arg0>``:Extend the specified existing session ID per the rest of the given request.

* ``--shrink <arg0>``: Shrink the specified existing session ID per the rest of the given request.

* ``--no-shell``: Immediately exit after allocating resources, without running a command.

* ``--begin <arg0>``: Direct the scheduler to defer allocation until the specified time.

* ``-I``| ``--immediate <arg0>``: Exit if resources are not available within the time period specified.

* ``-d`` | ``--dependency <arg0>``: Defer the start of this session until the specified dependencies have successfully completed.

* ``--do-not-wait``: Submit the allocation request to the scheduler, but do not wait for the allocation to be assigned. Intended for use when submitting a job for batch execution.


EXIT STATUS
-----------

Returns 0 for success, non-zero error code if a problem occurred. Note that success does not necessarily translate to success of the requested operation, depending upon provided options, but may instead mean that the request has been accepted for processing.

The command outputs the integer session ID of the resulting allocation, or if executed with the ``--do-not-wait`` option, a message indicating
if the request has been accepted for processing. Any command line arguments following the provided
options are taken as the job script or executable to be run within the allocation (once assigned) and its attendant
argv array. In this case, the allocation will terminate once the provide job script or executable completes.


EXAMPLES
--------

Examples of using this command.

.. seealso::
   :ref:`openpmix(5) <man5-openpmix>`
