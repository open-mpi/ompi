.. _label-install-required-support-libraries:

Required support libraries
==========================

Open MPI uses the following support libraries:

#. `Hardware Locality (hwloc)
   <https://www.open-mpi.org/projects/hwloc/>`_: This library is
   required; Open MPI will not build without it.

#. `Libevent <https://libevent.org/>`_: This library is required; Open
   MPI will not build without it.

#. `PMIx <https://pmix.org/>`_: This library is required; Open MPI
   will not build without it.

#. `PRRTE <https://github.com/openpmix/prrte>`_: This library is
   optional in some environments.  PRRTE provides Open MPI's
   full-featured ``mpirun`` / ``mpiexec`` MPI application launchers
   (the two are identical; they are symbolic links to the same executable).

   * If your environment uses another MPI application launcher
     (e.g., Slurm users can use the ``srun`` launcher to "direct
     launch" Open MPI applications), then the use of PRRTE is
     optional.
   * If your environment has no other MPI application launcher, then
     you need to install PRRTE and build Open MPI with PRRTE
     support.

Since these support libraries are fundamental to Open MPI's operation,
they are directly incorporated into Open MPI's configure, build, and
installation process.  More on this below.

Library dependencies
--------------------

These support libraries have dependencies upon each other:

.. The "source code" for this figure is simple a PPTX file by the same
   name in this same directory.  If you ever need to edit this image,
   edit the PPTX, export it to PNG, and then trim the whitespace from
   the sides of the image.

.. figure:: required-support-libraries-dependency-graph.png
   :align: center

   Open MPI required support library dependency graph.

The higher-level boxes depend on the lower-level boxes.  Specifically:

* Open MPI depends on PRRTE, PMIx, Hwloc, and Libevent (i.e.,
  everything).
* PRRTE depends on PMIx, Hwloc, and Libevent (i.e., everything except
  Open MPI).
* PMIx depends on Hwloc and Libevent.
* Hwloc does not depend on anything.
* Libevent does not depend on anything.

At run time, it is critical that the run-time linker loads *exactly
one copy* of each of these libraries.

.. note:: The required support libraries can have other dependencies,
          but for simplicitly and relevance to building Open MPI,
          those other dependencies are not discussed here.

Potential problems
------------------

Problems can (will) arise if multiple different copies of the above
shared libraries are loaded into a single process.  For example,
consider if:

* Loading the Open MPI shared library causes the loading of Libevent
  shared library vA.B.C.
* But then the subsequent loading of the PMIx shared library causes
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
   invoking Open MPI's ``configure`` script.

Not all package managers provide all of the required support
libraries. But even if your package manager installs |mdash| for
example |mdash| only Libevent and Hwloc, that somewhat simplifies the
final Open MPI configuration, and therefore avoids some potentially
erroneous configurations.

How ``configure`` finds the required libraries
----------------------------------------------

In an attempt to strike a balance between end-user convenience and
flexibility, Open MPI bundles these four required support libraries in
its official distribution tarball.

Generally, if Open MPI cannot find a required support library, it will
automatically configure, build, install, and use its bundled version
as part of the main Open MPI configure, build, and installation
process.

Put differently: Open MPI's ``configure`` script will examine the
build machine and see if it can find each of the required support
header files and libraries.  If it cannot find them, it will attempt
to fall back and use the corresponding bundled support library
instead.

.. important:: Note, however, that ``configure`` is smart enough to
   understand the dependencies between the required support libraries.

   Specifically: If ``configure`` finds the development headers and
   libraries for a given support library already installed on the
   system, then it will ignore both the corresponding bundled support
   library, *and it will also ignore all bundled support libraries
   that are below it in the dependency graph shown above.*

Build example 1
^^^^^^^^^^^^^^^

``configure`` finds the PRRTE development headers and libraries in
``/usr/local``.  This will cause the following to occur:

#. ``configure`` will ignore the PRRTE library that is bundled in the
   Open MPI source tree and will use the PRRTE that is already
   installed in ``/usr/local``.
#. ``configure`` will also ignore the bundled PMIx, Hwloc, and
   Libevent libraries in the Open MPI source tree.

   * If ``configure`` is unable to find header files and libraries for
     PMIx, Hwloc, and Libevent elsewhere on the build machine (i.e.,
     assumedly the same PMIx, Hwloc, and Libevent than the PRRTE in
     ``/usr/local`` is using), this is an error: ``configure`` will
     abort, and therefore refuse to build Open MPI.

Build example 2
^^^^^^^^^^^^^^^

``configure`` does *not* find PRRTE on the build machine, but *does*
find PMIx development headers and libraries in ``/opt/local``.  This
will cause the following to occur:

#. ``configure`` will set up to build the PRRTE library that is
   bundled in the Open MPI source tree.
#. ``configure`` will ignore the PMIx library that is bundled in the
   Open MPI source tree and will use the PMIx that is already
   installed in ``/opt/local``.
#. ``configure`` will also ignore the bundled Hwloc and Libevent
   libraries in the Open MPI source tree.

   * If ``configure`` is unable to find header files and libraries for
     Hwloc and Libevent elsewhere on the build machine (i.e.,
     assumedly the same Hwloc and Libevent than the PMIx in
     ``/opt/local`` is using), this is an error: ``configure`` will
     abort, and therefore refuse to build Open MPI.

Build example 3
^^^^^^^^^^^^^^^

``configure`` only finds the development headers and libraries for
Libevent on the build machine.  This will cause the following to
occur:

#. ``configure`` will set up to build the PRRTE, PMIx, and Hwloc
   libraries that are bundled in the Open MPI source tree.
#. ``configure`` will ignore the Libevent library that is bundled in
   the Open MPI source tree and will use the Libevent that is already
   installed.


Overriding ``configure`` behavior
---------------------------------

If ``configure``'s default searching behavior is not sufficient for
your environment, you can use :ref:`command line options to override
its default behavior
<label-building-ompi-cli-options-support-libraries>`.

For example, if PMIx and/or PRRTE are installed such that the default
header file and linker search paths will not find them, you can
provide command line options telling Open MPI's ``configure`` where to
search.  Here's an example ``configure`` invocation where PMIx and
PRRTE have both been installed to ``/opt/open-mpi-stuff``:

.. code-block:: sh

   ./configure --prefix=$HOME/openmpi-install \
       --with-pmix=/opt/open-mpi-stuff \
       --with-prrte=/opt/open-mpi-stuff ...

As another example, if you do not have root-level privileges to use
the OS / environment package manager, and if you have a simple MPI
application (e.g., that has no external library dependencies), you may
wish to configure Open MPI something like this:

.. code-block:: sh

   ./configure --prefix=$HOME/openmpi-install \
       --with-libevent=internal --with-hwloc=internal \
       --with-pmix=internal --with-prrte=internal ...

The ``internal`` keywords force ``configure`` to use all four bundled
versions of the required libraries.

.. danger:: Be very, very careful when overriding ``configure``'s
   default search behavior for these libraries.  Remember the critical
   requirement: that Open MPI infrastructure and applications load
   *exactly one copy* of each support library.  For simplicity, it may
   be desireable to ensure to use exactly the support libraries that
   Open MPI was compiled and built against.

   For example, using the Open MPI installed from the sample
   ``configure`` line (above), you may want to prefix your run-time
   linker search path (e.g., ``LD_LIBRARY_PATH`` on Linux) with
   ``$HOME/openmpi-install/lib``.  This will ensure that linker finds
   the four support libraries from your Open MPI installation tree,
   even if other copies of the same support libraries are present
   elsewhere on your system.

(Strong) Advice for packagers
-----------------------------

If you are an Open MPI packager, we **strongly** suggest that your
Open MPI package should not include Hwloc, Libevent, PMIx, or PRRTE.
Instead, it should depend on independently-built versions of these
packages.

You may wish to configure Open MPI with something like the
following:

.. code-block:: sh

   ./configure --with-libevent=external --with-hwloc=external \
       --with-pmix=external --with-prrte=external ...

The ``external`` keywords will force ``configure`` to ignore all the
bundled libraries and only look for external versions of these support
libraries.  This also has the benefit of causing ``configure`` to fail
if it cannot find the required support libraries outside of the Open
MPI source tree |mdash| a good sanity check to ensure that your
package is correctly relying on the independently-built and installed
versions.

:ref:`See this section
<label-building-ompi-cli-options-support-libraries>` for more
information about the required support library ``--with-FOO`` command
line options.
