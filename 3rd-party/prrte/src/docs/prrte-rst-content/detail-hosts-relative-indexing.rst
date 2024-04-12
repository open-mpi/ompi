.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Relative host indexing
======================

Hostfile and ``--host`` specifications can also be made using relative
indexing. This allows a user to stipulate which hosts are to be used
for a given app context without specifying the particular host name,
but rather its relative position in the allocation.

This can probably best be understood through consideration of a few
examples. Consider the case where a DVM is comprised of a set of nodes
named ``foo1``, ``foo2``, ``foo3``, ``foo4``. The user wants the first
app context to have exclusive use of the first two nodes, and a second
app context to use the last two nodes. Of course, the user could
printout the allocation to find the names of the nodes allocated to
them and then use ``--host`` to specify this layout, but this is
cumbersome and would require hand-manipulation for every invocation.

A simpler method is to utilize PRRTE's relative indexing capability to
specify the desired layout. In this case, a command line containing:

.. code::

   --host +n1,+n2 ./app1 : --host +n3,+n4 ./app2

would provide the desired pattern. The ``+`` syntax indicates that the
information is being provided as a relative index into the existing
allocation. Two methods of relative indexing are supported:

* ``+n#``: A relative index into the allocation referencing the ``#``
  node. PRRTE will substitute the ``#`` node in the allocation

* ``+e[:#]``: A request for ``#`` empty nodes |mdash| i.e., PRRTE is
  to substitute this reference with nodes that have not yet been used
  by any other app_context. If the ``:#`` is not provided, PRRTE will
  substitute the reference with all empty nodes. Note that PRRTE does
  track the empty nodes that have been assigned in this manner, so
  multiple uses of this option will result in assignment of unique
  nodes up to the limit of the available empty nodes. Requests for
  more empty nodes than are available will generate an error.

Relative indexing can be combined with absolute naming of hosts in any
arbitrary manner, and can be used in hostfiles as well as with the
``--host`` command line option. In addition, any slot specification
provided in hostfiles will be respected |mdash| thus, a user can
specify that only a certain number of slots from a relative indexed
host are to be used for a given app context.

Another example may help illustrate this point. Consider the case
where the user has a hostfile containing:

.. code::

   dummy1 slots=4
   dummy2 slots=4
   dummy3 slots=4
   dummy4 slots=4
   dummy5 slots=4

This may, for example, be a hostfile that describes a set of
commonly-used resources that the user wishes to execute applications
against. For this particular application, the user plans to map
byslot, and wants the first two ranks to be on the second node of any
allocation, the next ranks to land on an empty node, have one rank
specifically on ``dummy4``, the next rank to be on the second node of the
allocation again, and finally any remaining ranks to be on whatever
empty nodes are left. To accomplish this, the user provides a hostfile
of:

.. code::

   +n2 slots=2
   +e:1
   dummy4 slots=1
   +n2
   +e

The user can now use this information in combination with PRRTE's
sequential mapper to obtain their specific layout:

.. code::

   <launcher> --hostfile dummyhosts --hostfile mylayout --prtemca rmaps seq ./my_app

which will result in:

.. code::

   rank0 being mapped to dummy3
   rank1 to dummy1 as the first empty node
   rank2 to dummy4
   rank3 to dummy3
   rank4 to dummy2 and rank5 to dummy5 as the last remaining unused nodes

Note that the sequential mapper ignores the number of slots arguments
as it only maps one rank at a time to each node in the list.

If the default round-robin mapper had been used, then the mapping
would have resulted in:

* ranks 0 and 1 being mapped to dummy3 since two slots were specified
* ranks 2-5 on dummy1 as the first empty node, which has four slots
* rank6 on dummy4 since the hostfile specifies only a single slot from
  that node is to be used
* ranks 7 and 8 on dummy3 since only two slots remain available
* ranks 9-12 on dummy2 since it is the next available empty node and
  has four slots
* ranks 13-16 on dummy5 since it is the last remaining unused node and
  has four slots

Thus, the use of relative indexing can allow for complex mappings to
be ported across allocations, including those obtained from automated
resource managers, without the need for manual manipulation of scripts
and/or command lines.
