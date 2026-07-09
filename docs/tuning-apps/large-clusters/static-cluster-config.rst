Static cluster configurations
=============================

Clusters rarely change from day-to-day, and large clusters rarely
change at all.  If you know your cluster's configuration, there are
several steps you can take to both reduce Open MPI's memory footprint
and reduce the launch time of large-scale applications.  These steps
use a combination of build-time configuration options to eliminate
components |mdash| making Open MPI's libraries smaller and avoiding the
work of registering and querying components that will never be used
|mdash| as well as run-time MCA parameters to select the components to
use by default for most users.

.. note:: Since Open MPI v5.0, components are, by default, compiled
   directly into Open MPI's core libraries (for example,
   ``libopen-pal`` and ``libmpi``) rather than being built as separate
   dynamically-loaded plugins (DSOs).  Even when a component is compiled
   in this way, it is still registered and queried at run time so that
   each framework can decide which of its components to use.  The
   techniques on this page |mdash| not building components you do not
   need, and choosing framework defaults up front |mdash| therefore
   still reduce both memory footprint and startup work.  See
   :ref:`Components ("plugins"): static or DSO?
   <label-install-packagers-dso-or-not>` for more about the static and
   DSO build models and how to select between them.

One way to save memory is to avoid building components that will never
be selected on your system.  Every component that is built into Open
MPI is registered and tested at run time to decide whether it should be
selected, unless MCA parameters restrict which components are
considered.  If you know that a component can build on your system but,
due to your cluster's configuration, will never actually be selected,
then it is best to configure Open MPI to not build that component by
using the ``--enable-mca-no-build`` configure option.

For example, if you know that your system will only use the ``ob1``
component of the PML framework, then you can ``no_build`` all the
others.  This both makes the resulting libraries smaller and avoids the
startup cost of registering and querying components that would never be
used.

In some cases, however, you may want to keep a framework's other
components built |mdash| so that users retain the option to select them
|mdash| even though most users will use a single default.  For example,
you may want to keep all of the ``pml`` framework's components built (so
that users can still select ``ucx`` or ``cm`` when appropriate), even
though the vast majority of users will use the default ``ob1``
component.  This means you have to allow the system to build the other
components, even though they may rarely be used.

You can still save launch time and memory, though, by setting the
``pml = ob1`` MCA parameter in the default MCA parameter file.  This
tells Open MPI to consider only the ``ob1`` component at startup, rather
than registering and querying every ``pml`` component, but still allows
users to override the setting on their command line or in their
environment |mdash| so no functionality is lost, and you save some
memory and time.
