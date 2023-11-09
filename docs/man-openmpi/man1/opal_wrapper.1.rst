.. _man1-opal_wrapper:


opal_wrapper
============

.. include_body

opal_wrapper |mdash| Back-end Open MPI wrapper command


DESCRIPTION
-----------

``opal_wrapper`` is not meant to be called directly by end users. It
is automatically invoked as the back-end by the Open MPI wrapper
commands such as: ``mpicc``, ``mpicxx``, and ``mpifort``
(and its legacy/deprecated aliases ``mpif77`` and ``mpif90``).

Some Open MPI installations may have additional wrapper commands,
and/or have renamed the wrapper compilers listed above to avoid
executable name conflicts with other MPI implementations. Hence, you
may also have wrapper compilers installed including the following
names: ``mpifort.openmpi`` (and the legacy/deprecated aliases
``mpif90.openmpi`` and ``mpif77.openmpi``), ``mpicxx.openmpi``,
``mpicc.openmpi``.

.. seealso::
   The following may exist depending on your particular Open MPI
   installation:
   :ref:`mpicc(1) <man1-mpicc>`,
   :ref:`mpicxx(1) <man1-mpicxx>`,
   :ref:`mpifort(1) <man1-mpifort>`
