Tuning Collectives
==================

Open MPI's ``coll`` framework provides a number of components implementing
collective communication, including: ``han``, ``libnbc``, ``self``, ``ucc`` ``base``,
``sync``, ``xhc``, ``accelerator``, ``basic``, ``ftagree``, ``inter``, ``portals4``,
and ``tuned``.  Some of these components may not be available depending on how
Open MPI was compiled and what hardware is available on the system.  A run-time
decision based on each component's self reported priority, selects which
component will be used.  These priorities may be adjusted on the command line
or with any of the other usual ways of setting MCA variables, giving us a way
to influence or override component selection.  In the end, which of the
available components is selected depends on a number of factors such as the
underlying hardware and the whether or not a specific collective is provided by
the component as not all components implement all collectives.  However, there
is always a fallback ``base`` component that steps in and takes over when another
component fails to provide an implementation.

The remainder of this section describes the tuning options available in the
``tuned`` collective component.

Fixed, Forced, and Dynamic Decisions
------------------------------------
Open MPI's ``tuned`` collective component has three modes of operation, *fixed
decision*, *forced algorithm*, and *dynamic decision*.  Since different
collective algorithms perform better in different situations, the purpose of
these modes is, when a collective is called, to select an appropriate
algorithm.

In the *fixed decision* mode a decision tree, essentially a large set of nested
if-then-else-if blocks with baked in comm and message size thresholds derived
by measuring performance on existing clusters, selects one of a number of
available algorithms for the specific collective being called.  *Fixed
decision* is the default.  The *fixed decision* mode can work well if your
cluster hardware is similar to the systems profiled when constructing the
decision tree.  However, in some cases the *fixed decision* rules can yield
less than ideal performance. For instance, when your hardware differs
substantially from that which was used to derive the fixed rules decision
tree.

In the *dynamic decision* mode a user can provide a set of rules encoded in an
ASCII file that tell the ``tuned`` component which algorithm should be for one or
more collectives as a function of communicator and message size.  For the
collectives which rules are not provided the *fixed decision* mode is used.
More detail about the dynamic decision mode and rules file can be found in
section :ref:`Rules File<RulesFile>`.

In the *forced algorithm* mode one can specify an algorithm for a specific
collective on the command line. This is discussed in detail in section
:ref:`Forcing an Algorithm<ForceAlg>`.

.. _ForceAlg:

Forcing an Algorithm
--------------------

The simplest method of tuning is to force the ``tuned`` collective component to
use a specific algorithm for all calls to a specific collective. This can be
done on the command line, or by any of the other usual ways of setting MCA
variables.  For example, the following command line sets the algorithm to
linear for all MPI_Alltoall calls in the entire run.

.. code-block:: sh

   shell$ mpirun ... --mca coll_tuned_use_dynamic_rules 1 \
                     --mca coll_tuned_alltoall_algorithm 1 ...

When an algorithm is selected this way, the *fixed decision* rules associated
with that collective are short circuited.  This is an easy way to select a
collective algorithm and is most effective when there is a single communicator
and message sizes are static with in the run. On the other hand, when the
message size for a given collective varies during the run, or there are
multiple communicators of different sizes, the effectiveness of this approach
may be limited. The :ref:`rules file <RulesFile>` provides a more comprehensive
and flexible approach to tuning.  For collectives where an algorithm is not
forced on the command line, the *fixed decision* rules are used.

.. _RulesFile:

Dynamic Decisions and the Rules File
------------------------------------

Given that the best choice of algorithm for a given collective depends on a
number of factors only known at run time, and that some of these factors may
vary within a run, setting an algorithm on the command line often is an
ineffective means of tuning.  The rules file provides a means of choosing
an algorithm at run time based on communicator and message size.  The rules
file can be specified on the command line, or the other usual ways to set MCA
variables. The file is parsed during initialization and takes affect there
after.

.. code-block:: sh

   shell$ mpirun ... --mca coll_tuned_use_dynamic_rules 1 \
                     --mca coll_tuned_dynamic_rules_filename /path/to/my_rules.json ...

The loaded set of rules then are used to select the algorithm
to use based on the collective, the communicator size, and the message size.
Collectives for which rules have not be specified in the file will make use of
the *fixed decision* rules as usual.

Starting with Open MPI 6.0, dynamic tuning files can be specified in JSON
format, although the classic format will still be accepted.  A converter script
is also available to transfer classic format files into JSON.

The JSON format can be checked using the schema in
`docs/tuning-apps/tuned_dynamic_file_schema.json`.  If your editor supports it,
this schema may provide validation of your file along with helpful tooltips for
each variable.

An example file is shown here:

.. code-block:: json

    {
        "$schema": "tuned_schema.json",
        "rule_file_version" : 3,
        "module" : "tuned",
        "collectives" : {
            "allreduce" :
            [
                {
                    "comm_size_min" : 64,
                    "comm_size_max" : 128,
                    "rules" : [
                        {
                            "msg_size_min" : 512,
                            "msg_size_max" : 511999,
                            "alg" : 2,
                        },
                        {
                            "msg_size_min" : 512000,
                            "msg_size_max" : "inf",
                            "alg" : "recursive_doubling",
                            "reqs" : 8
                        }
                    ]
                }
            ]
        }
    }

In this toy example the MPI_Allreduce collective (indicated by the `allreduce`
field) has two algorithms that will only be used on communicators with between
64 and 128 ranks.  Additionally, those rules only apply to certain message
sizes.  All others communicator sizes or message sizes fall back to the default
set of rules, and collectives other than MPI_Allreduce are not affected.

Unlike in the classic file format, there is no need to specify a default rule or
specify rules in increasing order.  Overlapping message sizes or communicator
sizes are allowed, and won't emit warnings.

The process for selecting the matching rule is a simple first-match principle.
During communicator creation, the first set of communicator-rules which
satisfies the requirements (`comm_size_min`/`comm_size_max`) is selected. Then,
during each collective call, the message size is used to find the first matching
entry in the "rules" list.

The algorithm selected is indicated by the `alg` field.  It may be either an
integer mapping to the classic file format, or a string.  In both cases, the
value is checked against the appropriate coll_tuned_<collectived>_algorithm MCA
parameter, and un-recognized values will cause the rule to be ignored.


Classic file format:

.. code-block:: sh
   :linenos:

   rule-file-version-2
   1   # num of collectives
   3   # collective ID
   1   # number of comm sizes
   #=====================
   64   # comm size
   14   # number of rules
   # Bytes   alg topo segs reqs
   #----------------------
   0            0 0 0 0
   512000       4 0 0 64
   1536000      4 0 0 64
   3072000      4 0 0 64
   6144000      4 0 0 64
   12288000     4 0 0 16
   24576000     4 0 0 16
   49152000     4 0 0 16
   98304000     4 0 0 16
   196608000    4 0 0 8
   393216000    4 0 0 8
   786432000    4 0 0 1
   1572864000   4 0 0 1
   2621440000   0 0 0 0

The rules file effectively defines, for one or more collectives, a function of
two variables, which given communicator and message size, returns an algorithm
id to use for the call.  This mechanism allows one to specify for each
collective, an algorithm for any number of ranges of message and communicator
sizes.  As communicators are constructed, a search of the rules table is made
using the communicator size to select a set of message size rules to be used
with that communicator.  Later as the collective is invoked, a search of the
message size rules associated with the communicator is made. The rule with the
nearest (less than) matching message size specifies the algorithm that is used.
The actual definition of *message size* is dependent on the collective in
question, see the section on :ref:`collectives and algorithms<CollectivesAndAlgorithms>`
for details.

One may provide rules for as many collectives, communicator sizes, and message
sizes as desired. Simply repeat the sections as needed and adjust the relevant
count parameters.  One must always provide a rule for message size of zero.
Message size rules are expected in ascending order. The last parameters in the
message size rule may or may not be used and have different meaning depending
on the collective and algorithm. The first two parameters in the rule following
the algorithm ID, `topo` and `segment size`, are always required. In version 2
of the file format a third parameter, `max requests`, may also be provided. A
release of Open MPI at least v5.0.7 is required for version 2 features.

The file format version specifier, `rule-file-version-N` where N is an integer
greater or equal to 1, should appear on the first line of the file.  If the
version specifier is not present, the file format is assumed to be version 1.
Version 2 or greater is required to use the `max requests` parameter. Open MPI
releases older than v5.0.7 do not support the file format version
identifier. When using older releases of Open MPI do not include a version
specifier and do not use the `max requests` parameter in message size rules.

.. _CollectivesAndAlgorithms:

Collectives and their Algorithms
--------------------------------
The following table lists the collectives
implemented by the ``tuned`` collective component along with the enumeration value identifying it.
It is this value that must be used in the rules file when specifying a set of rules.
The definition of *message size* is dependent on the collective and is given in the table.
Tables describing the algorithms available for each collective, and there identifiers, are linked.

.. csv-table:: Collectives
   :header: "Collective", "Id", "Message Size"
   :widths: 20, 10, 65

   :ref:`Allgather<Allgather>`, 0, "datatype size * comm size * number of elements in send buffer"
   :ref:`Allgatherv<Allgatherv>`, 1, "datatype size * sum of number of elements that are to be received from each process (sum of recvcounts)"
   :ref:`Allreduce<Allreduce>`, 2, "datatype size * number of elements in send buffer"
   :ref:`Alltoall<Alltoall>`, 3, "datatype size * comm size * number of elements to send to each process"
   :ref:`Alltoallv<Alltoallv>`, 4, "not used"
   :ref:`Barrier<Barrier>`, 6, "not used"
   :ref:`Bcast<Bcast>`, 7, "datatype size * number of entries in buffer"
   :ref:`Exscan<Exscan>`, 8, "datatype size * comm size"
   :ref:`Gather<Gather>`, 9, "datatype size * comm size * number of elements in send buffer"
   :ref:`Reduce<Reduce>`, 11, "datatype size * number of elements in send buffer"
   :ref:`Reduce_scatter<Reduce_scatter>`, 12, "datatype size * sum of number of elements in result distributed to each process (sum of recvcounts)"
   :ref:`Reduce_scatter_block<Reduce_scatter_block>`, 13, "datatype size * comm size * element count per block"
   :ref:`Scan<Scan>`, 14, "datatype size * comm size"
   :ref:`Scatter<Scatter>`, 15, "datatype size * number of elements in send buffer"

.. _Allgather:

Allgather (Id=0)
~~~~~~~~~~~~~~~~

.. csv-table:: Allgather Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "linear", "..."
   2, "bruck-k-fanout", "..."
   3, "recursive_doubling", "..."
   4, "ring", "..."
   5, "neighbor", "..."
   6, "two_proc", "..."
   7, "sparbit", "..."
   8, "direct-messaging", "..."

.. _Allgatherv:

Allgatherv (Id=1)
~~~~~~~~~~~~~~~~~

.. csv-table:: Allgatherv Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "default", "..."
   2, "bruck", "..."
   3, "ring", "..."
   4, "neighbor", "..."
   5, "two_proc", "..."
   6, "sparbit", "..."

.. _Allreduce:

Allreduce (Id=2)
~~~~~~~~~~~~~~~~

.. csv-table:: Allreduce Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "basic_linear", "..."
   2, "nonoverlapping", "..."
   3, "recursive_doubling", "..."
   4, "ring", "..."
   5, "segmented_ring", "..."
   6, "rabenseifner", "..."
   7, "allgather_reduce", "..."

.. _Alltoall:

Alltoall (Id=3)
~~~~~~~~~~~~~~~

.. csv-table:: Alltoall Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "linear", "Launches all non-blocking send/recv pairs and wait for them to complete."
   2, "pairwise", "For comm size P, implemented as P rounds of blocking MPI_Sendrecv"
   3, "modified_bruck", "An algorithm exploiting network packet quantization to achieve O(log) time complexity. Typically best for very small message sizes."
   4, "linear_sync", "Keep N non-blocking MPI_Isend/Irecv pairs in flight at all times. N is set by the coll_tuned_alltoall_max_requests MCA variable."
   5, "two_proc", "An implementation tailored for alltoall between 2 ranks, otherwise it is not used."

.. _Alltoallv:

Alltoallv (Id=4)
~~~~~~~~~~~~~~~~

.. csv-table:: Alltoallv Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "basic_linear", "..."
   2, "pairwise", "..."

.. _Barrier:

Barrier (Id=6)
~~~~~~~~~~~~~~

.. csv-table:: Barrier Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "linear", "..."
   2, "double_ring", "..."
   3, "recursive_doubling", "..."
   4, "bruck", "..."
   5, "two_proc", "..."
   6, "tree", "..."

.. _Bcast:

Bcast (Id=7)
~~~~~~~~~~~~

.. csv-table:: Bcast Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "basic_linear", "..."
   2, "chain", "..."
   3, "pipeline", "..."
   4, "split_binary_tree", "..."
   5, "binary_tree", "..."
   6, "binomial", "..."
   7, "knomial", "..."
   8, "scatter_allgather", "..."
   9, "scatter_allgather_ring", "..."

.. _Exscan:

Exscan (Id=8)
~~~~~~~~~~~~~

.. csv-table:: Exscan Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "linear", "..."
   2, "recursive_doubling", "..."

.. _Gather:

Gather (Id=9)
~~~~~~~~~~~~~

.. csv-table:: Gather Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "basic_linear", "..."
   2, "binomial", "..."
   3, "linear_sync", "..."

.. _Reduce:

Reduce (Id=11)
~~~~~~~~~~~~~~

.. csv-table:: Reduce Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "linear", "..."
   2, "chain", "..."
   3, "pipeline", "..."
   4, "binary", "..."
   5, "binomial", "..."
   6, "in-order_binary", "..."
   7, "rabenseifner", "..."
   8, "knomial", "..."

.. _Reduce_scatter:

Reduce_scatter (Id=12)
~~~~~~~~~~~~~~~~~~~~~~

.. csv-table:: Reduce_scatter Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "non-overlapping", "..."
   2, "recursive_halving", "..."
   3, "ring", "..."
   4, "butterfly", "..."

.. _Reduce_scatter_block:

Reduce_scatter_block (Id=13)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. csv-table:: Reduce_scatter_block Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "basic_linear", "..."
   2, "recursive_doubling", "..."
   3, "recursive_halving", "..."
   4, "butterfly", "..."

.. _Scan:

Scan (Id=14)
~~~~~~~~~~~~

.. csv-table:: Scan Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "linear", "..."
   2, "recursive_doubling", "..."

.. _Scatter:

Scatter (Id=15)
~~~~~~~~~~~~~~~

.. csv-table:: Scatter Algorithms
   :header: "Id", "Name", "Description"
   :widths: 10, 25, 60

   0, "ignore", "Use fixed rules"
   1, "basic_linear", "..."
   2, "binomial", "..."
   3, "linear_nb", "..."

