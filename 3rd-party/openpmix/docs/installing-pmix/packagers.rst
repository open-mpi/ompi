Advice for packagers
====================

.. _label-install-packagers-dso-or-not:

Components ("plugins"): DSO or no?
----------------------------------

PMIx contains a large number of components (sometimes called
"plugins") to effect different types of functionality in PMIx.  For
example, some components provide support for networks and
may link against specialized libraries to do so.

PMIx |opmix_ver| has two ``configure``-time defaults regarding the
treatment of components that may be of interest to packagers:

#. PMIx's libraries default to building as shared libraries
   (vs. static libraries).  For example, on Linux, PMIx will
   default to building ``libpmix.so`` (vs. ``libpmix.a``).

   .. note:: See the descriptions of ``--disable-shared`` and
             ``--enable-static`` :ref:`in this section
             <label-building-installation-cli-options>` for more
             details about how to change this default.

             Also be sure to :ref:`see this warning about building
             static apps <label-building-fully-static-apps>`.

#. PMIx will default to including its components in its libraries
   (as opposed to being compiled as dynamic shared objects, or DSOs).
   For example, ``libpmix.so`` on Linux systems will contain the OPA
   PNET component, instead of the OPA PNET being compiled into
   ``mca_pnet_opa.so`` and dynamically opened at run time via
   ``dlopen(3)``.

   .. note:: See the descriptions of ``--enable-mca-dso`` and
             ``--enable-mca-static`` :ref:`in this section
             <label-building-installation-cli-options>` for more
             details about how to change this defaults.

A side effect of these two defaults is that all the components
included in the PMIx libraries will bring their dependencies with
them.  For example (on Linux), if the XYZ PNET component
requires ``libXYZ.so``, then these defaults mean that
``libpmix.so`` will depend on ``libXYZ.so``.  This dependency will
likely be telegraphed into the PMIx binary package that includes
``libpmix.so``.

Conversely, if the XYZ PNET component was built as a DSO, then |mdash|
assuming no other parts of PMIx require ``libXYZ.so`` |mdash|
``libpmix.so`` would *not* be dependent on ``libXYZ.so``.  Instead, the
``mca_pnet_xyz.so`` DSO would have the dependency upon ``libXYZ.so``.

Packagers can use these facts to potentially create multiple binary
PMIx packages, each with different dependencies by, for example,
using ``--enable-mca-dso`` to selectively build some components as
DSOs and leave the others included in their respective PMIx
libraries.

.. code:: sh

   # Build all the "pnet" components as DSOs (all other
   # components will default to being built in their respective
   # libraries)
   shell$ ./configure --enable-mca-dso=pnet ...

This allows packaging ``$libdir`` as part of the "main" PMIx
binary package, but then packaging
``$libdir/pmix/mca_pnet_*.so`` as sub-packages.  These
sub-packages may inherit dependencies on their own.
Users can always install the "main" PMIx
binary package, and can install the additional "pnet" PMIx
binary sub-package if they actually have network hardware
installed (which will cause the installation of additional
dependencies).
