.. _label-running-selecting-framework-components:

Selecting which Open MPI components are used at run time
========================================================

Each MCA framework has a top-level MCA parameter that helps guide
which components are selected to be used at run-time.  Specifically,
every framework has an MCA parameter of the same name that can be used
to *include* or *exclude* components from a given run.

For example, the ``btl`` MCA parameter is used to control which BTL
components are used.  It takes a comma-delimited list of component
names, and may be optionally prefixed with ``^``.  For example:

.. note:: The Byte Transfer Layer (BTL) framework is used as the
          underlying network transports with the `ob1` Point-to-point
          Messaging Layer (PML) component.

.. code-block:: sh

   # Tell Open MPI to include *only* the BTL components listed here and
   # implicitly ignore all the rest:
   shell$ mpirun --mca btl self,sm,usnic ...

   # Tell Open MPI to exclude the tcp and uct BTL components
   # and implicitly include all the rest
   shell$ mpirun --mca btl ^tcp,uct ...

Note that ``^`` can *only* be the prefix of the *entire*
comma-delimited list because the inclusive and exclusive behavior are
mutually exclusive.  Specifically, since the exclusive behavior means
"use all components *except* these", it does not make sense to mix it
with the inclusive behavior of not specifying it (i.e., "use all of
these components").  Hence, something like this:

.. code-block:: sh

   shell$ mpirun --mca btl self,sm,usnic,^tcp ...

does not make sense |mdash| and will cause an error |mdash| because it
says "use only the ``self``, ``sm``, and ``usnic`` components" but
also "use all components except ``tcp``".  These two statements
clearly contradict each other.
