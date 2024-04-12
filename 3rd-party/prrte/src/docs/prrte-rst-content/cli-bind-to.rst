.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

By default, processes are bound to individual CPUs (either COREs or
HWTHREADs, as defined by default or by user specification for the
job). On nodes that are OVERSUBSCRIBEd (i.e., where the number of
procs exceeds the number of assigned slots), the default is to not
bind the processes.

.. note:: Processes from prior jobs that are already executing on a
          node are not "unbound" when a new job mapping results in the
          node becoming oversubscribed.

Binding is performed to the first available specified object type
within the object where the process was mapped. In other words,
binding can only be done to the mapped object or to a resource
located beneath that object.

An object is considered completely consumed when the number of
processes bound to it equals the number of CPUs within it. Unbound
processes are not considered in this computation. Additional
processes cannot be mapped to consumed objects unless the
``OVERLOAD`` qualifier is provided via the ``--bind-to`` command
line option.

Note that directives and qualifiers are case-insensitive
and can be shortened to the minimum number of characters
to uniquely identify them. Thus, ``L1CACHE`` can be given
as ``l1cache`` or simply as ``L1``.

Supported binding directives include:

* ``NONE`` does not bind the processes

* ``HWTHREAD`` binds each process to a single hardware
  thread/ This requires that hwthreads be treated
  as independent CPUs (i.e., that either the ``HWTCPUS``
  qualifier be provided to the ``map-by`` option or
  that ``hwthreads`` be designated as CPUs by default).

* ``CORE`` binds each process to a single core. This
  can be done whether ``hwthreads`` or ``cores`` are being
  treated as independent CPUs provided that mapping
  is performed at the core or higher level.

* ``L1CACHE`` binds each process to all the CPUs in
  an ``L1`` cache.

* ``L2CACHE`` binds each process to all the CPUs in
  an ``L2`` cache

* ``L3CACHE`` binds each process to all the CPUs in
  an ``L3`` cache

* ``NUMA`` binds each process to all the CPUs in a ``NUMA``
  region

* ``PACKAGE`` binds each process to all the CPUs in a ``PACKAGE``

Any directive can include qualifiers by adding a colon (:) and any
combination of one or more of the following to the ``--bind-to``
option:

* ``OVERLOAD`` indicates that objects can have more
  processes bound to them than CPUs within them

* ``IF-SUPPORTED`` indicates that the job should continue to
  be launched and executed even if binding cannot be
  performed as requested.

.. note:: Directives and qualifiers are case-insensitive.
          ``OVERLOAD`` is the same as ``overload``.
