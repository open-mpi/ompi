TCP
===

Using the TCP BTL for MPI messages
----------------------------------

Open MPI generally uses the ``tcp`` BTL automatically when:

#. The ``tcp`` BTL is available at run time (which it should be on most
   POSIX-like systems), and
#. A higher-performance network is not available.

When the ``tcp`` BTL is used, it is typically also used (automatically)
with the ``self`` and ``sm`` BTLs for process-loopback and node-loopback
communication, respectively.

To guarantee that the ``tcp``, ``sm``, and ``self`` BTLs are used, you
can specify them explicitly on the ``mpirun`` command line:

.. code-block:: sh

   shell$ mpirun --mca pml ob1 --mca btl tcp,sm,self ...

.. warning:: Failure to specify the ``sm`` BTL will likely result in
             lower performance when Open MPI uses the TCP network stack
             to send to peers on the same host.

.. warning:: Failure to specify the ``self`` BTL may result in Open MPI
             being unable to complete send-to-self scenarios (meaning
             that your program will run fine until a process tries to
             send to itself).

Coexisting with a high-speed network
------------------------------------

If you have both an IP network and at least one high-speed network
(such as InfiniBand), you do not need to disable the TCP BTL.
Following the so-called "Law of Least Astonishment," Open MPI assumes
that you will likely want to use only the high-speed network(s) for MPI
message passing, so the ``tcp`` BTL component senses this and
automatically deactivates itself.

That said, Open MPI may still use TCP for setup and teardown
information, so you will see traffic across your IP network during
startup and shutdown of your MPI job.  This is normal and does not
affect the MPI message-passing channels.

Listing tunable parameters
--------------------------

The ``ompi_info`` command can display all the parameters available for
the ``tcp`` BTL component (that is, the component that uses TCP for MPI
communication):

.. code-block:: sh

   shell$ ompi_info --param btl tcp --level 9

The IP loopback interface
-------------------------

Open MPI usually does *not* use the operating system IP loopback
interface.  In general message-passing usage, there are two scenarios
in which the IP loopback interface could be used:

#. Sending a message from one process to itself, and
#. Sending a message from one process to another process on the same
   machine.

The ``tcp`` BTL does not handle send-to-self scenarios; instead, the
``self`` BTL is used for all send-to-self MPI communication.  This lets
all Open MPI BTL components avoid special-case code for send-to-self
and also avoids using less efficient loopback network stacks (such as
the IP loopback device).  The ``self`` component uses its own
mechanisms and does not use operating system network interfaces such as
the IP loopback interface.

When sending to other processes on the same machine, Open MPI defaults
to using a shared-memory BTL (``sm``).  If the shared-memory BTL has
been deactivated, then |mdash| depending on what other BTL components
are available |mdash| it is possible that the ``tcp`` BTL will be
chosen for on-node message passing, in which case the IP loopback
device will likely be used.  This is not the default, however: either
shared memory must fail to start up properly, or the user must
specifically request not to use the shared-memory BTL.  If you do need
to use the IP loopback device, the ``tcp`` BTL can be configured to use
it by setting the ``btl_tcp_if_include`` MCA parameter to ``lo`` or
``127.0.0.0/32`` (or whatever the local naming scheme is on your
system).  For example:

.. code-block:: sh

   shell$ mpirun --mca btl_tcp_if_include lo ...

Interpreting TCP-related errors
-------------------------------

TCP-related errors are usually reported by Open MPI in a message
similar to these:

.. code-block::

   btl_tcp_endpoint.c:572:mca_btl_tcp_endpoint_complete_connect: connect() failed with errno=113
   mca_btl_tcp_frag_send: writev failed with errno=104

If an ``errno`` number is displayed with no explanation string, you can
look up what that specific error number means on your operating system.
On Linux, you can use the ``perror`` command:

.. code-block:: sh

   # See what errno 113 is
   shell$ perror 113
   OS error code 113:  No route to host

   # See what errno 104 is
   shell$ perror 104
   OS error code 104:  Connection reset by peer

Two types of errors are commonly reported to the Open MPI users'
mailing list:

#. **No route to host:** These errors *usually* mean that there are
   multiple IP interfaces available and they do not obey Open MPI's
   assumptions about routability.  See :ref:`how Open MPI determines
   routability <faq-tcp-routability>` and :ref:`selecting which IP
   interfaces to use <faq-tcp-selection>` for more information.

#. **Connection reset by peer:** These errors *usually* occur after
   ``MPI_Init`` has completed, and typically indicate that an MPI
   process has died unexpectedly (for example, due to a catastrophic
   error such as a segmentation fault).  The message indicates that a
   peer MPI process tried to write to the now-dead MPI process and
   failed.

.. _faq-tcp-selection:

Selecting which IP interfaces to use
------------------------------------

In some HPC environments, it is not uncommon to have multiple IP
interfaces on each node |mdash| for example, one "slow" IP network used
for control information (such as a batch scheduler, a networked
filesystem, and/or interactive logins) and another "fast" IP network
(or networks) intended for parallel applications to use during their
runs.  Some operating systems may also have virtual interfaces for
communicating with virtual machines.

Unless otherwise specified, Open MPI greedily uses all "up" IP networks
that it can find, per its :ref:`reachability computations
<faq-tcp-routability>`, and tries to connect to all peers *on demand*
(Open MPI does not open sockets to all of its MPI peers during
``MPI_Init`` |mdash| see :ref:`sockets opened during MPI_Init
<faq-tcp-sockets>`).  If you want MPI jobs to not use specific IP
networks |mdash| or not use any IP networks at all |mdash| then you
must tell Open MPI.

.. warning:: Aggressively using all "up" interfaces can cause problems
             in some cases.  For example, if you have a machine with a
             local-only interface (such as the loopback device, or a
             virtual-machine bridge device that can only be used *on
             that machine* and cannot be used to communicate with MPI
             processes on other machines), you will likely need to tell
             Open MPI to ignore these networks.

             Open MPI usually ignores loopback devices by default, but
             *other local-only devices must be manually ignored.* Users
             have reported cases where RHEL6 automatically installed a
             ``virbr0`` device for Xen virtualization.  This interface
             was automatically given an IP address in the 192.168.1.0/24
             subnet and marked as "up".  Since Open MPI saw this
             192.168.1.0/24 "up" interface in all MPI processes on all
             nodes, it assumed that network was usable for MPI
             communication.  This is obviously incorrect, and it led to
             MPI applications hanging when they tried to send or receive
             MPI messages.

There are several ways to control which interfaces Open MPI uses:

#. To prevent Open MPI from using TCP for MPI communication at all, set
   the ``btl`` MCA parameter accordingly.  You can either *exclude* the
   TCP component or *include* only other components:

   .. code-block:: sh

      # Exclude the TCP BTL component (implicitly including all others)
      shell$ mpirun --mca btl ^tcp ...

      # Include only the listed BTL components
      # (tcp is not listed, and therefore will not be used)
      shell$ mpirun --mca btl self,sm ...

#. To use TCP for MPI communication but restrict it to (or from)
   certain networks, use the ``btl_tcp_if_include`` or
   ``btl_tcp_if_exclude`` MCA parameter (only one of the two should be
   set).  The value can be a comma-delimited list of network
   interfaces.  For example:

   .. code-block:: sh

      # Do not use the lo and eth0 interfaces (and implicitly use the
      # rest).  Per the description above, IP loopback and all
      # local-only devices *must* be included in an exclude list.
      shell$ mpirun --mca btl_tcp_if_exclude lo,eth0 ...

      # Use only the eth1 and eth2 interfaces (and implicitly ignore
      # the rest)
      shell$ mpirun --mca btl_tcp_if_include eth1,eth2 ...

#. You can also specify subnets in the include or exclude lists in CIDR
   notation.  For example:

   .. code-block:: sh

      # Only use the 192.168.1.0/24 and 10.10.0.0/16 subnets for MPI
      # communication
      shell$ mpirun --mca btl_tcp_if_include 192.168.1.0/24,10.10.0.0/16 ...

   .. note:: You must specify the CIDR notation for a given network
             precisely.  For example, if you have two IP networks
             10.10.0.0/24 and 10.10.1.0/24, Open MPI will not recognize
             either of them if you specify "10.10.0.0/16".

.. warning:: If you use the ``btl_tcp_if_include`` and
             ``btl_tcp_if_exclude`` MCA parameters to shape the behavior
             of the TCP BTL for MPI communication, you may also
             need/want to investigate the corresponding PRRTE parameters
             that control use of network interfaces by the runtime (for
             example, communication setup and coordination during
             :ref:`MPI_Init` and :ref:`MPI_Finalize`), using the
             :ref:`prte_info(1) <prrte:man1-prte_info>` and
             :ref:`pmix_info(1) <pmix:man1-pmix_info>` commands.

Note that the Open MPI runtime uses TCP for control messages |mdash|
such as data exchange between ``mpirun(1)`` and the MPI processes,
rendezvous information during :ref:`MPI_Init`, and so on |mdash| even if
the ``tcp`` BTL component is disabled.

.. _faq-tcp-sockets:

Sockets opened during MPI_Init
------------------------------

Although Open MPI is likely to open multiple TCP sockets during
``MPI_Init``, the ``tcp`` BTL component *does not open one socket per
MPI peer process during* ``MPI_Init``.  Open MPI opens sockets as they
are required, so the first time a process sends a message to a peer and
there is no TCP connection between the two, Open MPI automatically opens
a new socket.

As a result, you should not have scalability issues (such as running
out of per-process file descriptors) when running large numbers of
processes, provided your parallel application is sparse in its
communication with peers.

Recommended Linux kernel TCP parameters
---------------------------------------

Everyone has different opinions on this, and the best settings also
depend on your exact hardware and environment.  The following are
general guidelines that some users have found helpful.

#. ``net.ipv4.tcp_syn_retries``: Some Linux systems have very large
   initial connection timeouts |mdash| they retry sending SYN packets
   many times before determining that a connection cannot be made.  If
   MPI is going to fail to make socket connections, it is better for it
   to fail somewhat quickly (minutes vs. hours), so you might want to
   reduce this value; your mileage may vary.

#. ``net.ipv4.tcp_keepalive_time``: Some MPI applications send an
   initial burst of MPI messages (over TCP) and then send nothing for
   long periods of time (for example, embarrassingly parallel
   applications).  Linux may decide that these dormant TCP sockets are
   dead because it has seen no traffic on them for a long time.  You
   might therefore need to lengthen the TCP inactivity timeout.  Many
   Linux systems default to 7,200 seconds; increase it if necessary.

#. Increase TCP buffering for 10G or 40G Ethernet.  Many Linux
   distributions come with good buffering presets for 1G Ethernet, but
   in a datacenter/HPC cluster with 10G or 40G Ethernet NICs, this
   amount of kernel buffering is typically insufficient.  Here is a set
   of parameters that some have used for good 10G/40G TCP bandwidth:

   * ``net.core.rmem_max``: 16777216
   * ``net.core.wmem_max``: 16777216
   * ``net.ipv4.tcp_rmem``: 4096 87380 16777216
   * ``net.ipv4.tcp_wmem``: 4096 65536 16777216
   * ``net.core.netdev_max_backlog``: 30000
   * ``net.core.rmem_default``: 16777216
   * ``net.core.wmem_default``: 16777216
   * ``net.ipv4.tcp_mem``: '16777216 16777216 16777216'
   * ``net.ipv4.route.flush``: 1

Each of the above is a Linux kernel parameter that can be set in
several ways:

#. You can change the running kernel via the ``/proc`` filesystem:

   .. code-block:: sh

      shell# cat /proc/sys/net/ipv4/tcp_syn_retries
      5
      shell# echo 6 > /proc/sys/net/ipv4/tcp_syn_retries

#. You can use the ``sysctl`` command:

   .. code-block:: sh

      shell# sysctl net.ipv4.tcp_syn_retries
      net.ipv4.tcp_syn_retries = 5
      shell# sysctl -w net.ipv4.tcp_syn_retries=6
      net.ipv4.tcp_syn_retries = 6

#. You can set them persistently (across reboots) by adding entries in
   ``/etc/sysctl.conf``:

   .. code-block:: sh

      shell$ grep tcp_syn_retries /etc/sysctl.conf
      net.ipv4.tcp_syn_retries = 6

#. Your Linux distribution may also support putting individual files in
   ``/etc/sysctl.d`` (even if that directory does not yet exist), which
   is actually better practice than putting them in
   ``/etc/sysctl.conf``.  For example:

   .. code-block:: sh

      shell$ cat /etc/sysctl.d/my-tcp-settings
      net.ipv4.tcp_syn_retries = 6

.. _faq-tcp-routability:

How Open MPI determines routability
-----------------------------------

Open MPI assumes that all interfaces are routable as long as they have
the same address family (IPv4 or IPv6).  It uses graph theory, giving
each possible connection a weight depending on the quality of the
connection, which allows the library to select the best connections
between nodes.  This method also supports striping, but prevents more
than one connection to any interface.

The quality of a connection is defined as follows, with a higher number
meaning a better connection.  Note that a connection consisting of a
private address and a public address is given the weight
``PRIVATE_DIFFERENT_NETWORK``.

.. code-block::

               NO_CONNECTION = 0
   PRIVATE_DIFFERENT_NETWORK = 1
   PRIVATE_SAME_NETWORK      = 2
   PUBLIC_DIFFERENT_NETWORK  = 3
   PUBLIC_SAME_NETWORK       = 4

An example best illustrates how two processes on two different nodes
would connect.  Here we have two nodes with a variety of interfaces:

.. code-block::

            Node A                Node B
      ----------------       ----------------
     |      lo0       |     |      lo0       |
     | 127.0.0.1/8    |     | 127.0.0.1/8    |
     |                |     |                |
     |      eth0      |     |      eth0      |
     | 10.8.47.1/24   |     | 10.8.47.2/24   |
     |                |     |                |
     |      eth1      |     |      eth1      |
     | 192.168.1.1/24 |     | 192.168.1.2/24 |
     |                |     |                |
     |      eth2      |     |                |
     | 192.168.2.2/24 |     |                |
      ----------------      ------------------

From these two nodes, the software builds a bipartite graph that shows
all the possible connections with all the possible weights.  The
``lo0`` interfaces are excluded because the ``btl_tcp_if_exclude`` MCA
parameter is set to ``lo`` by default.  Here is what all the possible
connections with their weights look like:

.. code-block::

         Node A       Node B
   eth0 --------- 2 -------- eth0
          ------- 1 -------- eth1

   eth1 --------- 1 -------- eth0
          ------- 2 -------- eth1

   eth2 --------- 1 -------- eth0
          ------- 1 -------- eth1

The library then examines all the connections and picks the optimal
ones.  This leaves two connections established between the two nodes.

If you are curious about the actual ``connect()`` calls being made by
the processes, run with ``--mca btl_base_verbose 30``.  This can be
useful if you notice your job hanging and believe it may be the library
trying to make connections to unreachable hosts:

.. code-block:: sh

   # Here is an example with some of the output deleted for clarity.
   # One can see the connections that are attempted.
   shell$ mpirun --mca btl self,sm,tcp --mca btl_base_verbose 30 -n 2 -host NodeA,NodeB a.out
   [...snip...]
   [NodeA:18003] btl: tcp: attempting to connect() to address 10.8.47.2 on port 59822
   [NodeA:18003] btl: tcp: attempting to connect() to address 192.168.1.2 on port 59822
   [NodeB:16842] btl: tcp: attempting to connect() to address 192.168.1.1 on port 44500
   [...snip...]

If you want more details about the theory behind the connection code,
you can find the background story in `this IEEE paper
<https://ieeexplore.ieee.org/document/4476565>`_.

When Open MPI closes TCP sockets
--------------------------------

In general, Open MPI does not close TCP sockets; however, there are
some exceptions.  Although TCP sockets are opened lazily (MPI
connections / TCP sockets are only opened on demand, as opposed to
opening all possible sockets between MPI peer processes during
``MPI_Init``), they are never closed unless the MPI world or MPI
sessions are explicitly finalized or disconnected.  For example, if the
MPI world is finalized, all TCP sockets are closed.  If a spawned MPI
process is disconnected, all TCP sockets to any parent MPI process are
closed.  Similarly, if an MPI session is disconnected, all TCP sockets
to any child MPI processes are closed.

Interfaces with multiple IP addresses
--------------------------------------

In general, Open MPI does not support an IP interface that has more than
one IP address.  For example, if the output from your ``ifconfig`` has a
single IP device with multiple IP addresses like this:

.. code-block::

   0: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc mq state UP qlen 1000
      link/ether 00:18:ae:f4:d2:29 brd ff:ff:ff:ff:ff:ff
      inet 192.168.0.3/24 brd 192.168.0.255 scope global eth0:1
      inet 10.10.0.3/24 brd 10.10.0.255 scope global eth0
      inet6 fe80::218:aef2:29b4:2c4/64 scope link
         valid_lft forever preferred_lft forever

(note the two ``inet`` lines), then Open MPI will be unable to use this
device.

Virtual IP interfaces
---------------------

Open MPI does not support virtual IP interfaces.  For example, if the
output of your ``ifconfig`` has both ``eth0`` and ``eth0:0``, Open MPI
will get confused if you use the TCP BTL, and may hang or otherwise act
unpredictably.  Note that using ``btl_tcp_if_include`` or
``btl_tcp_if_exclude`` to avoid using the virtual interface will *not*
solve the issue.

Using multiple TCP connections
------------------------------

Open MPI can use multiple TCP connections between any pair of MPI
processes, striping large messages across the connections.  The
``btl_tcp_links`` parameter sets how many TCP connections are
established between MPI processes.

Note that this may not improve application performance for common use
cases of nearest-neighbor exchanges when there are many MPI processes
on each host.  In those cases, there are already many TCP connections
between any two hosts (because of the many processes all communicating),
so the extra TCP connections are likely just consuming extra resources
and adding work to the MPI implementation.

However, for highly multi-threaded applications where there are only one
or two MPI processes per host, the ``btl_tcp_links`` option may improve
TCP throughput considerably.

Limiting the time spent establishing TCP connections
-----------------------------------------------------

You can set the ``btl_tcp_recv_timeout`` and
``btl_tcp_handshake_timeout`` MCA parameters to limit the time spent
establishing TCP connections.  The difference between the two is subtle
but important: ``btl_tcp_recv_timeout`` is the timeout for one receive
operation, while ``btl_tcp_handshake_timeout`` is the timeout for the
entire handshake (that is, the exchange of the correct magic string and
process GUID).  These parameters can be used to avoid deadlocks in
adversarial situations where external processes (for example, processes
that are not part of any MPI job) try to connect to the Open MPI process
and hold the connection socket open without sending the proper handshake
information.

The default values are 250,000 usec for ``btl_tcp_recv_timeout`` and
1,000,000 usec for ``btl_tcp_handshake_timeout``.  For example:

.. code-block:: sh

   shell$ mpirun --mca btl_tcp_recv_timeout 1000000 ...
   shell$ mpirun --mca btl_tcp_handshake_timeout 1000000 ...

Nagle's algorithm and TCP_NODELAY
---------------------------------

By default, the TCP BTL disables Nagle's algorithm |mdash| that is, it
sets the ``TCP_NODELAY`` socket option |mdash| which favors low latency
for applications driven by waves of small messages.  Using Nagle's
algorithm can increase short-message latency.

This behavior is controlled by the ``btl_tcp_use_nagle`` MCA parameter,
which defaults to 0 (Nagle's algorithm disabled, ``TCP_NODELAY`` set).
Set it to 1 to use Nagle's algorithm (clearing ``TCP_NODELAY``); this is
rarely desirable for latency-sensitive workloads.
