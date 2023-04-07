Reducing wireup time
====================

Open MPI's run-time uses an *out-of-band* (OOB) communication
subsystem to pass messages during the launch, initialization, and
termination stages for the job. These messages allow ``mpirun`` to tell
its daemons what processes to launch, and allow the daemons in turn to
forward stdio to ``mpirun``, update ``mpirun`` on process status, etc.

The OOB uses TCP sockets for its communication, with each daemon
opening a socket back to ``mpirun`` upon startup. In a large cluster,
this can mean thousands of connections being formed on the node where
``mpirun`` resides, and requires that ``mpirun`` actually process all
these connection requests. ``mpirun`` defaults to processing
connection requests sequentially |mdash| so on large clusters, a
backlog can be created that can cause remote daemons to timeout
waiting for a response.

Fortunately, Open MPI provides an alternative mechanism for processing
connection requests that helps alleviate this problem. Setting the MCA
parameter ``oob_tcp_listen_mode`` to ``listen_thread`` causes
``mpirun`` to startup a separate thread dedicated to responding to
connection requests. Thus, remote daemons receive a quick response to
their connection request, allowing ``mpirun`` to deal with the message
as soon as possible.

.. error:: TODO This seems very out of date.  We should have content
           about PMIx instant on.

This parameter can be included in the default MCA parameter file,
placed in the user's environment, or added to the ``mpirun`` command
line.  See :ref:`this FAQ entry <label-running-setting-mca-param-values>`
for more details on how to set MCA parameters.
