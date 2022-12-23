TCP
===

.. error:: TODO This section needs to be converted from FAQ Q&A style
           to regular documentation style.

How do I specify to use the IP network for MPI messages?
--------------------------------------------------------

Open MPI will generally automatically use the ``tcp`` BTL when:

#. The ``tcp`` BTL is  available at run time (which it should be on
   most POSIX-like systems), and
#. A higher-performance network is not available

When the ``tcp`` BTL is used, it is typically also (automatically)
used with the ``self`` and ``sm`` BTLs for process-loopback and
node-loopback communication, respectively.

If you want to guarantee that the ``tcp``, ``sm``, and ``self`` BTLs
are used, you can explicitly specify them on the ``mpirun`` command
line:

.. code-block:: sh

   shell$ mpirun --mca pml ob1 --mca btl tcp,sm,self ...

.. warning:: Failure to specify the ``sm`` BTL will likely result in
             lower performance when Open MPI uses the TCP network
             stack to send to peers on the same host.

.. warning:: Failure to specify the ``self`` BTL may result in Open
             MPI being unable to complete send-to-self scenarios
             (meaning that your program will run fine until a process
             tries to send to itself).

/////////////////////////////////////////////////////////////////////////

But wait |mdash| I'm using a high-speed network.  Do I have to disable the TCP BTL?
-----------------------------------------------------------------------------------

No.  Following the so-called "Law of Least Astonishment", Open MPI
assumes that if you have both an IP network and at least one
high-speed network (such InfiniBand), you will likely only want to use
the high-speed network(s) for MPI message passing.  Hence, the ``tcp``
BTL component will sense this and automatically deactivate itself.

That being said, Open MPI may still use TCP for setup and teardown
information |mdash| so you'll see traffic across your IP network during
startup and shutdown of your MPI job.  This is normal and does not
affect the MPI message passing channels.

/////////////////////////////////////////////////////////////////////////

How do I know what MCA parameters are available for tuning MPI performance?
---------------------------------------------------------------------------

The ``ompi_info`` command can display all the parameters
available for the ``tcp`` BTL component (i.e., the component that uses
TCP for MPI communications):

.. code-block:: sh

   shell$ ompi_info --param btl tcp --level 9

/////////////////////////////////////////////////////////////////////////

Does Open MPI use the IP loopback interface?
--------------------------------------------

Usually not.

In general message passing usage, there are two scenarios where using
the operating system IP loopback interface could be used:

#. Sending a message from one process to itself
#. Sending a message from one process to another process on the same
   machine

The TCP BTL does not handle "send-to-self" scenarios in Open MPI;
indeed, it is not even capable of doing so.  Instead, the ``self`` BTL
component is used for all send-to-self MPI communications.  Not only
does this allow all Open MPI BTL components to avoid special case code
for send-to-self scenarios, it also allows avoiding using inefficient
loopback network stacks (such as the IP loopback device).

Specifically: the ``self`` component uses its own mechanisms for
send-to-self scenarios; it does not use operating system network
interfaces such as the IP loopback interface.

When sending to other processes on the same machine, Open MPI will
default to using a shared memory BTL (``sm``).  If the user has
deactivated these BTLs, depending on what other BTL components are
available, it is possible that the TCP BTL will be chosen for message
passing to processes on the same node, in which case the IP loopback
device will likely be used.  But this is not the default; either
shared memory has to fail to startup properly or the user must
specifically request not to use the shared memory BTL.

/////////////////////////////////////////////////////////////////////////

I have multiple IP networks on some/all of my cluster nodes.  Which ones will Open MPI use?
-------------------------------------------------------------------------------------------

In general, Open MPI will greedily use all IP networks that
it finds per its :ref:`reachability computations <faq-tcp-routability>`.

To change this behavior, you can either specifically include certain
networks or specifically exclude certain networks.  :ref:`See this FAQ
entry <faq-tcp-selection>` for more details.

/////////////////////////////////////////////////////////////////////////

I'm getting TCP-related errors.  What do they mean?

TCP-related errors are usually reported by Open MPI in a message
similar to these:

.. code-block::

   btl_tcp_endpoint.c:572:mca_btl_tcp_endpoint_complete_connect: connect() failed with errno=113
   mca_btl_tcp_frag_send: writev failed with errno=104

If an `errno` number is displayed with no explanation string, you can
see what that specific error number means on your operating system.
On Linux, you can use the ``perror`` command:

.. code-block:: sh

   # See what errno 113 is
   shell$ perror 113
   OS error code 113:  No route to host

   # See what errno 104 is
   shell$ perror 104
   OS error code 104:  Connection reset by peer

Two types of errors are commonly reported to the Open MPI user's
mailing list:

#. **No route to host:** These types of errors *usually* mean that
   there are multiple IP interfaces available and they do not obey
   Open MPI's assumptions about routability.  See :ref:`the TCP
   routability assumptions FAQ entry <faq-tcp-routability>` and
   :ref:`the TCP selection FAQ entry <faq-tcp-selection>` for more
   information.

#. **Connection reset by peer:** These types of errors *usually* occur
   after ``MPI_INIT`` has completed, and typically indicate that an
   MPI process has died unexpectedly (e.g., due to a catastrophic error
   such as a segmentation fault).  The specific error message
   indicates that a peer MPI process tried to write to the now-dead
   MPI process and failed.

/////////////////////////////////////////////////////////////////////////

.. _faq-tcp-selection:

How do I tell Open MPI which IP interfaces / networks to use?
-------------------------------------------------------------

In some HPC environments, it is not uncommon to have multiple IP
interfaces on each node |mdash| for example, one IP network may be
"slow" and used for control information such as a batch scheduler, a
networked filesystem, and/or interactive logins.  Another IP network
(or networks) may be "fast" and be intended for parallel applications
to use during their runs.  As another example, some operating systems
may also have virtual interfaces for communicating with virtual
machines.

Unless otherwise specified, Open MPI will greedily use all "up" IP
networks that it can find and try to connect to all peers *upon
demand* (i.e., Open MPI does not open sockets to all of its MPI peers
during ``MPI_INIT`` |mdash| see :ref:`this FAQ entry
<faq-tcp-sockets>` for more details).  Hence, if you want MPI jobs to
not use specific IP networks |mdash| or not use any IP networks at all
|mdash| then you need to tell Open MPI.

.. warning:: Aggressively using all "up" interfaces can cause problems
             in some cases.  For example, if you have a machine with a
             local-only interface (e.g., the loopback device, or a
             virtual-machine bridge device that can only be used *on
             that machine*, and cannot be used to communicate with MPI
             processes on other machines), you will likely need to
             tell Open MPI to ignore these networks.

             Open MPI usually ignores loopback devices by default, but
             *other local-only devices must be manually ignored.*
             Users have reported cases where RHEL6 automatically
             installed a ``virbr0`` device for Xen virtualization.
             This interface was automatically given an IP address in
             the 192.168.1.0/24 subnet and marked as "up".  Since Open
             MPI saw this 192.168.1.0/24 "up" interface in all MPI
             processes on all nodes, it assumed that that network was
             usable for MPI communications.  This is obviously
             incorrect, and it led to MPI applications hanging when
             they tried to send or receive MPI messages.

#. To disable Open MPI from using TCP for MPI communications, the
   ``tcp`` MCA parameter should be set accordingly.  You can either
   *exclude* the TCP component or *include* all other components.
   Specifically:

   .. code-block:: sh

      # This says to exclude the TCP BTL component
      # (implicitly including all others)
      shell$ mpirun --mca btl ^tcp...

      # This says to include only the listed BTL components
      # (tcp is not listed, and therefore will not be used)
      shell$ mpirun --mca btl self,vader,openib ...

#. If you want to use TCP for MPI communications, but want to restrict
   it from certain networks, use the ``btl_tcp_if_include`` or
   ``btl_tcp_if_exclude`` MCA parameters (only one of the two should
   be set).  The values of these parameters can be a comma-delimited
   list of network interfaces.  For example:

   .. code-block:: sh

      # This says to not use the eth0 and lo interfaces.
      # (and implicitly use all the rest).  Per the description
      # above, IP loopback and all local-only devices *must*
      # be included if the exclude list is specified.
      shell$ mpirun --mca btl_tcp_if_exclude lo,eth0 ...

      # This says to only use the eth1 and eth2 interfaces
      # (and implicitly ignore the rest)
      shell$ mpirun --mca btl_tcp_if_include eth1,eth2 ...

#. You can  also specify subnets  in the  include or exclude  lists in
   CIDR notation.  For example:

   .. code-block:: sh

      # Only use the 192.168.1.0/24 and 10.10.0.0/16 subnets for MPI
      # communications:
      shell$ mpirun --mca btl_tcp_if_include 192.168.1.0/24,10.10.0.0/16 ...


   .. note:: You must specify the CIDR notation for a given network
             precisely.  For example, if you have two IP networks
             10.10.0.0/24 and 10.10.1.0/24, Open MPI will not
             recognize either of them if you specify "10.10.0.0/16".

.. warning:: If you use the ``btl_tcp_if_include`` and
             ``btl_tcp_if_exclude`` MCA parameters to shape the
             behavior of the TCP BTL for MPI communications, you may
             also need/want to investigate the corresponding MCA
             parameters ``oob_tcp_if_include`` and
             ``oob_tcp_if_exclude``, which are used to shape non-MPI
             TCP-based communication (e.g., communications setup and
             coordination during ``MPI_INIT`` and ``MPI_FINALIZE``).

.. error:: TODO do corresponding OOB TCP params still exist in PMIx?

Note that Open MPI will still use TCP for control messages, such as
data between ``mpirun`` and the MPI processes, rendezvous information
during ``MPI_INIT``, etc.  To disable TCP altogether, you also need to
disable the ``tcp`` component from the OOB framework.

.. error:: TODO Is this possible in PMIx?  I doubt it...?

/////////////////////////////////////////////////////////////////////////

.. _faq-tcp-sockets:

Does Open MPI open a bunch of sockets during ``MPI_INIT``?
----------------------------------------------------------

Although Open MPI is likely to open multiple TCP sockets during
``MPI_INIT``, the ``tcp`` BTL component *does not open one socket per
MPI peer process during MPI_INIT.*  Open MPI opens sockets as they
are required |mdash| so the first time a process sends a message to a
peer and there is no TCP connection between the two, Open MPI will
automatically open a new socket.

Hence, you should not have scalability issues with running large
numbers of processes (e.g., running out of per-process file
descriptors) if your parallel application is sparse in its
communication with peers.

/////////////////////////////////////////////////////////////////////////

Are there any Linux kernel TCP parameters that I should set?
------------------------------------------------------------

Everyone has different opinions on this, and it also depends
on your exact hardware and environment.  Below are general guidelines
that some users have found helpful.

#. ``net.ipv4.tcp_syn_retries``: Some Linux systems have very large
   initial connection timeouts |mdash| they retry sending SYN packets
   many times before determining that a connection cannot be made.  If
   MPI is going to fail to make socket connections, it would be better
   for them to fail somewhat quickly (minutes vs. hours).  You might
   want to reduce this value to a smaller value; YMMV.

#. ``net.ipv4.tcp_keepalive_time``: Some MPI applications send an
   initial burst of MPI messages (over TCP) and then send nothing for
   long periods of time (e.g., embarrassingly parallel applications).
   Linux may decide that these dormant TCP sockets are dead because it
   has seen no traffic on them for long periods of time.  You might
   therefore need to lengthen the TCP inactivity timeout.  Many Linux
   systems default to 7,200 seconds; increase it if necessary.

#. Increase TCP buffering for 10G or 40G Ethernet.  Many Linux
   distributions come with good buffering presets for 1G Ethernet.  In
   a datacenter/HPC cluster with 10G or 40G Ethernet NICs, this amount
   of kernel buffering is typically insufficient.  Here's a set of
   parameters that some have used for good 10G/40G TCP bandwidth:

   * ``net.core.rmem_max``: 16777216
   * ``net.core.wmem_max``: 16777216
   * ``net.ipv4.tcp_rmem``: 4096 87380 16777216
   * ``net.ipv4.tcp_wmem``: 4096 65536 16777216
   * ``net.core.netdev_max_backlog``: 30000
   * ``net.core.rmem_default``: 16777216
   * ``net.core.wmem_default``: 16777216
   * ``net.ipv4.tcp_mem``: '16777216 16777216 16777216'
   * ``net.ipv4.route.flush``: 1

   Each of the above items is a Linux kernel parameter that can be set
   in multiple different ways.

   #. You can change the running kernel via the ``/proc`` filesystem:

      .. code-block:: sh

         shell# cat /proc/sys/net/ipv4/tcp_syn_retries
         5
         shell# echo 6 > /proc/sys/net/ipv4/tcp_syn_retries

   #. You can also use the ``sysctl`` command:

      .. code-block:: sh

         shell# sysctl net.ipv4.tcp_syn_retries
         net.ipv4.tcp_syn_retries = 5
         shell# sysctl -w net.ipv4.tcp_syn_retries=6
         net.ipv4.tcp_syn_retries = 6

   #. Or you can set them by adding entries in ``/etc/sysctl.conf``,
      which are persistent across reboots:

      .. code-block:: sh

         shell$ grep tcp_syn_retries /etc/sysctl.conf
         net.ipv4.tcp_syn_retries = 6

   #. Your Linux distro may also support putting individual files in
      ``/etc/sysctl.d`` (even if that directory does not yet exist),
      which is actually better practice than putting them in
      ``/etc/sysctl.conf``.  For example:

      .. code-block:: sh

         shell$ cat /etc/sysctl.d/my-tcp-settings
         net.ipv4.tcp_syn_retries = 6

/////////////////////////////////////////////////////////////////////////

.. _faq-tcp-routability:

How does Open MPI know which IP addresses are routable to each other?
---------------------------------------------------------------------

Open MPI assumes that all interfaces are routable as long as they have
the same address family, IPv4 or IPv6.  We use graph theory and give
each possible connection a weight depending on the quality of the
connection.  This allows the library to select the best connections
between nodes.  This method also supports striping but prevents more
than one connection to any interface.

The quality of the connection is defined as follows, with a higher
number meaning better connection.  Note that when giving a weight to a
connection consisting of a private address and a public address, it
will give it the weight of ``PRIVATE_DIFFERENT_NETWORK``.

.. code-block::

               NO_CONNECTION = 0
   PRIVATE_DIFFERENT_NETWORK = 1
   PRIVATE_SAME_NETWORK      = 2
   PUBLIC_DIFFERENT_NETWORK  = 3
   PUBLIC_SAME_NETWORK       = 4

An example will best illustrate how two processes on two different
nodes would connect up.  Here we have two nodes with a variety of
interfaces:

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

From these two nodes, the software builds up a bipartite graph that
shows all the possible connections with all the possible weights.  The
*lo0* interfaces are excluded as the ``btl_tcp_if_exclude`` MCA parameter
is set to *lo* by default.  Here is what all the possible connections
with their weights look like.

.. code-block::

         Node A       Node B
   eth0 --------- 2 -------- eth0
          ------- 1 -------- eth1

   eth1 --------- 1 -------- eth0
          ------- 2 -------- eth1

   eth2 --------- 1 -------- eth0
          ------- 1 -------- eth1

The library then examines all the connections and picks the optimal
ones.  This leaves us with two connections being established between
the two nodes.

If you are curious about the actual ``connect()`` calls being made by
the processes, then you can run with ``--mca btl_base_verbose 30``.
This can be useful if you notice your job hanging and believe it may
be the library trying to make connections to unreachable hosts.

.. code-block:: sh

   # Here is an example with some of the output deleted for clarity.
   # One can see the connections that are attempted.
   shell$ mpirun --mca btl self,sm,tcp --mca btl_base_verbose 30 -n 2 -host NodeA,NodeB a.out
   [...snip...]
   [NodeA:18003] btl: tcp: attempting to connect() to address 10.8.47.2 on port 59822
   [NodeA:18003] btl: tcp: attempting to connect() to address 192.168.1.2 on port 59822
   [NodeB:16842] btl: tcp: attempting to connect() to address 192.168.1.1 on port 44500
   [...snip...]

In case you want more details about the theory behind the connection
code, you can find the background story in `this IEEE paper
<https://ieeexplore.ieee.org/document/4476565>`_.

/////////////////////////////////////////////////////////////////////////

Does Open MPI ever close TCP sockets?
-------------------------------------

In general, no.

Although TCP sockets are opened "lazily" (meaning that MPI
connections / TCP sockets are only opened upon demand |mdash| as opposed to
opening all possible sockets between MPI peer processes during
``MPI_INIT``), they are never closed.

/////////////////////////////////////////////////////////////////////////

Does Open MPI support IP interfaces that have more than one IP address?
-----------------------------------------------------------------------

In general, no.

For example, if the output from your ``ifconfig`` has a single IP device
with multiple IP addresses like this:

.. code-block::

   0: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc mq state UP qlen 1000
      link/ether 00:18:ae:f4:d2:29 brd ff:ff:ff:ff:ff:ff
      inet 192.168.0.3/24 brd 192.168.0.255 scope global eth0:1
      inet 10.10.0.3/24 brf 10.10.0.255 scope global eth0
      inet6 fe80::218:aef2:29b4:2c4/64 scope link
         valid_lft forever preferred_lft forever

(note the two ``inet`` lines in there)

Then Open MPI will be unable to use this device.

/////////////////////////////////////////////////////////////////////////

Does Open MPI support virtual IP interfaces?
--------------------------------------------

No.

For example, if the output of your ``ifconfig`` has both ``eth0`` and
``eth0:0``, Open MPI will get confused if you use the TCP BTL, and
may hang or otherwise act unpredictably.

Note that using ``btl_tcp_if_include`` or ``btl_tcp_if_exclude`` to avoid
using the virtual interface will *not* solve the issue.

/////////////////////////////////////////////////////////////////////////

Can I use multiple TCP connections to improve network performance?
------------------------------------------------------------------

Open MPI can use multiple TCP connections between any pair of MPI
processes, striping large messages across the connections.  The
``btl_tcp_links`` parameter can be used to set how many TCP
connections should be established between MPI processes.

Note that
this may not improve application performance for common use cases of
nearest-neighbor exchanges when there many MPI processes on each host.  In
these cases, there are already many TCP connections between any two
hosts (because of the many processes all communicating), so the extra TCP
connections are likely just consuming extra resources and adding work
to the MPI implementation.

However, for highly multi-threaded applications, where there are only
one or two MPI processes per host, the ``btl_tcp_links`` option may
improve TCP throughput considerably.
