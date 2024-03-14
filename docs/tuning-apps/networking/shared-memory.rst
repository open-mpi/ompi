Shared Memory
=============

.. error:: TODO This section needs to be converted from FAQ Q&A style
           to regular documentation style.

What is the sm BTL?
-------------------

The ``sm`` BTL is a low-latency, high-bandwidth mechanism for
transferring data between two processes via shared memory.  This BTL
can only be used between processes executing on the same node.

.. note:: Between Open MPI version 1.8.0 and 4.1.x, the shared memory
          BTL was named ``vader``.  As of Open MPI version 5.0.0, the
          BTL has been renamed ``sm``.

.. warning:: In Open MPI version 5.0.x, the name ``vader`` is simply
             an alias for the ``sm`` BTL.  Similarly, all
             ``vader_``-prefixed MCA parameters are automatically
             aliased to their corresponding ``sm_``-prefixed MCA
             parameter.

             This alias mechanism is a legacy transition device, and
             will likely disappear in a future release of Open MPI.

/////////////////////////////////////////////////////////////////////////

How do I specify use of sm for MPI messages?
--------------------------------------------

Typically, it is unnecessary to do so;  OMPI will use the best BTL available
for each communication.

Nevertheless, you may use the MCA parameter ``btl``.  You should also
specify the ``self`` BTL for communications between a process and
itself.  Furthermore, if not all processes in your job will run on the
same, single node, then you also need to specify a BTL for internode
communications.  For example:

.. code-block:: sh

   shell$ mpirun --mca btl self,sm,tcp -n 16 ./a.out

/////////////////////////////////////////////////////////////////////////

How can I tune these parameters to improve performance?
-------------------------------------------------------

Mostly, the default values of the MCA parameters have already
been chosen to give good performance.  To improve performance further
is a little bit of an art.  Sometimes, it's a matter of trading off
performance for memory.

* ``btl_sm_eager_limit``: If message data plus header information fits
  within this limit, the message is sent "eagerly" |mdash| that is, a
  sender attempts to write its entire message to shared buffers
  without waiting for a receiver to be ready.  Above this size, a
  sender will only write the first part of a message, then wait for
  the receiver to acknowledge its readiness before continuing.  Eager
  sends *can* improve performance by decoupling senders from
  receivers.

* ``btl_sm_max_send_size``: Large messages are sent in fragments of
  this size.  Larger segments *can* lead to greater efficiencies,
  though they could perhaps also inhibit pipelining between sender and
  receiver.

* ``btl_sm_free_list_num``: This is the initial number of fragments on
  each (eager and max) free list.  The free lists can grow in response
  to resource congestion, but you can increase this parameter to
  pre-reserve space for more fragments.

/////////////////////////////////////////////////////////////////////////

Where is the shared memory mapped on the filesystem?

.. error:: TODO Is this correct?

The file will be in the OMPI session directory, which is typically
something like ``/tmp/openmpi-sessions-USERNAME@HOSTNAME/*``.
The file itself will have the name
``shared_mem_pool.HOSTNAME``.  For example, the full path could be
``/tmp/openmpi-sessions-johndoe@node0_0/1543/1/shared_mem_pool.node0``.

.. error:: TODO The filename above will certainly be wrong.

To place the session directory in a non-default location, use the MCA parameter
``orte_tmpdir_base``.

.. error:: TODO The MCA param name above is definitely wrong.
