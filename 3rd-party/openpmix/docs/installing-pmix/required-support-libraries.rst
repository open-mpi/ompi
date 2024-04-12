.. _label-install-required-support-libraries:

Required support libraries
==========================

PMIx requires the following support libraries with the minimum listed versions:

.. list-table::
   :header-rows: 1
   :widths: 10 10 25

   * - Library
     - Minimum version
     - Notes
   * - `Hardware Locality <https://www.open-mpi.org/projects/hwloc/>`_
     - |hwloc_min_version|
     - This library is required; PMIx will not build without it.
   * - `Libevent <https://libevent.org/>`_
     - |event_min_version|
     - Either libevent or libev must be provided
   * - `libev <https://metacpan.org/dist/EV/view/libev/ev.pod>`_
     - no specified minimum
     - Either libevent or libev must be provided

These support libraries are fundamental to PMIx's operation
and pretty universally available in all environments. Worst case,
they can easily be built from their respective source tarballs.

Library dependencies
--------------------

These support libraries do not have dependencies upon each other.
However, it often is true that another library being used by an
application, or the application itself, can include a dependency
on one or more of them.

At run time, it is critical that the run-time linker loads *exactly
one copy* of each of these libraries. It is therefore vital that
you have a clear understanding of the dependencies within your
application.

Potential problems
------------------

Problems can (will) arise if multiple different copies of the above
shared libraries are loaded into a single process.  For example,
consider if:

* Loading the PMIx shared library causes the loading of Libevent
  shared library vA.B.C.
* But then the subsequent loading of the FOO shared library causes
  the loading of Libevent shared library vX.Y.Z.

Since there are now two different versions of the Libevent shared
library loaded into the same process (yes, this can happen!),
unpredictable behavior can (will) occur.

Many variations on this same basic erroneous scenario are possible.
All of them are bad, and can be extremely difficult to diagnose.

Avoiding the problems
---------------------

A simple way to avoid these problems is to configure your system such
that it has exactly one copy of each of the required support libraries.

.. important:: If possible, use your OS / environment's package
   manager to install as many of these support libraries |mdash|
   including their development headers |mdash| as possible before
   invoking PMIx's ``configure`` script.

Not all package managers provide all of the required support
libraries. But even if your package manager installs |mdash| for
example |mdash| only one of Libevent and Hwloc, that somewhat simplifies the
final PMIx configuration, and therefore avoids some potentially
erroneous configurations.

Overriding ``configure`` behavior
---------------------------------

PMIx's ``configure`` will (minus any configure flags) attempt to
find the HWLOC and libevent packages in standard locations. Note
that PMIx by default will look for libevent (and not libev), though
either can be used. If a required package is not found and no flags
were given, the ``configure`` operation will exit with a suitable
error message.

If ``configure``'s default searching behavior is not sufficient for
your environment, you can use :ref:`command line options to override
its default behavior
<label-building-pmix-cli-options-required-support-libraries>`.

For example, if libevent and/or HWLOC are installed such that the default
header file and linker search paths will not find them, you can
provide command line options telling PMIx's ``configure`` where to
search.  Here's an example ``configure`` invocation where HWLOC
and libevent have both been installed under the ``/opt`` directory:

.. code-block:: sh

   ./configure --prefix=$HOME/pmix-install \
       --with-libevent=/opt/libevent \
       --with-hwloc=/opt/hwloc ...

.. danger:: Be very, very careful when overriding ``configure``'s
   default search behavior for these libraries.  Remember the critical
   requirement: that PMIx infrastructure and applications load
   *exactly one copy* of each support library.  For simplicity, it may
   be desirable to ensure to use exactly the support libraries that
   PMIx was compiled and built against.

   For example, using the PMIx installed from the sample
   ``configure`` line (above), you may want to prefix your run-time
   linker search path (e.g., ``LD_LIBRARY_PATH`` on Linux) with
   ``$HOME/pmix-install/lib:/opt/libevent/lib:/opt/hwloc/lib``.
   This will ensure that the linker finds
   the support libraries used by your PMIx installation tree,
   even if other copies of the same support libraries are present
   elsewhere on your system.
