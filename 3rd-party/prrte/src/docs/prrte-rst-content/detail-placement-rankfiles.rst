.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Rankfiles
=========

Another way to specify arbitrary mappings is with a rankfile, which
gives you detailed control over process binding as well.

Rankfiles are text files that specify detailed information about how
individual processes should be mapped to nodes, and to which
processor(s) they should be bound. Each line of a rankfile specifies
the location of one process. The general form of each line in the
rankfile is:

.. code::

   rank <N>=<hostname> slot=<slot list>

For example:

.. code::

   $ cat myrankfile
   rank 0=aa slot=10-12
   rank 1=bb slot=0,1,4
   rank 2=cc slot=1-2
   $ prun --host aa,bb,cc,dd --map-by rankfile:FILE=myrankfile ./a.out

Means that:

* Rank 0 runs on node aa, bound to logical cores 10-12.
* Rank 1 runs on node bb, bound to logical cores 0, 1, and 4.
* Rank 2 runs on node cc, bound to logical cores 1 and 2.

Similarly:

.. code::

   $ cat myrankfile
   rank 0=aa slot=1:0-2
   rank 1=bb slot=0:0,1,4
   rank 2=cc slot=1-2
   $ prun --host aa,bb,cc,dd --map-by rankfile:FILE=myrankfile ./a.out

Means that:

* Rank 0 runs on node aa, bound to logical package 1, cores 10-12 (the
  0th through 2nd cores on that package).
* Rank 1 runs on node bb, bound to logical package 0, cores 0, 1,
  and 4.
* Rank 2 runs on node cc, bound to logical cores 1 and 2.

The hostnames listed above are "absolute," meaning that actual
resolvable hostnames are specified. However, hostnames can also be
specified as "relative," meaning that they are specified in relation
to an externally-specified list of hostnames (e.g., by ``prun``'s
``--host`` argument, a hostfile, or a job scheduler).

The "relative" specification is of the form "``+n<X>``", where ``X``
is an integer specifying the Xth hostname in the set of all available
hostnames, indexed from 0. For example:

.. code::

   $ cat myrankfile
   rank 0=+n0 slot=10-12
   rank 1=+n1 slot=0,1,4
   rank 2=+n2 slot=1-2
   $ prun --host aa,bb,cc,dd --map-by rankfile:FILE=myrankfile ./a.out

All package/core slot locations are be specified as *logical*
indexes. You can use tools such as HWLOC's ``lstopo`` to find the
logical indexes of packages and cores.
