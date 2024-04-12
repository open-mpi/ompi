
.. _building-pmix-installation-location-label:

Installation location
=====================

A common environment to run PMIx is in a "Beowulf"-class or
similar cluster (e.g., a bunch of 1U servers in a bunch of racks).
Simply stated, PMIx can run on a group of servers or workstations
connected by a network.

This raises the question for PMIx system administrators: where to
install the PMIx binaries, header files, etc.?  This discussion
mainly addresses this question for homogeneous clusters (i.e., where
all nodes and operating systems are the same), although elements of
this discussion apply to heterogeneous clusters as well.

.. important:: For simplicity, the PMIx team *strongly* recommends
   that you install PMIx at the same path location on all nodes in
   your cluster.  This *greatly* simplifies the user experience of
   running jobs across multiple nodes in your cluster.

   It is *possible* to install PMIx in unique path locations in
   the different nodes in your cluster, but it is not *advisable*.

Filesystem types
----------------

There are two common approaches.

Network filesystem
^^^^^^^^^^^^^^^^^^

Have a common filesystem, such as NFS, between all the machines to be
used.  Install PMIx such that the installation directory is the
*same value* on each node.  This will *greatly* simplify user's shell
startup scripts (e.g., ``.bashrc``, ``.cshrc``, ``.profile`` etc.)
|mdash| the ``PATH`` can be set without checking which machine the
user is on.  It also simplifies the system administrator's job; when
the time comes to patch or otherwise upgrade PMIx, only one copy
needs to be modified.

For example, consider a cluster of four machines: ``inky``,
``blinky``, ``pinky``, and ``clyde``.

* Install PMIx on ``inky``'s local hard drive in the directory
  ``/opt/pmix-VERSION``.  The system administrator then mounts
  ``inky:/opt/pmix-VERSION`` on the remaining three machines, such
  that ``/opt/pmix-VERSION`` on all machines is effectively "the
  same".  That is, the following directories all contain the PMIx
  installation:

  .. code-block::

     inky:/opt/pmix-VERSION
     blinky:/opt/pmix-VERSION
     pinky:/opt/pmix-VERSION
     clyde:/opt/pmix-VERSION

* Install PMIx on ``inky``'s local hard drive in the directory
  ``/usr/local/pmix-VERSION``.  The system administrator then
  mounts ``inky:/usr/local/pmix-VERSION`` on *all four* machines in
  some other common location, such as ``/opt/pmix-VERSION`` (a
  symbolic link can be installed on ``inky`` instead of a mount point
  for efficiency).  This strategy is typically used for environments
  where one tree is NFS exported, but another tree is typically used
  for the location of actual installation.  For example, the following
  directories all contain the PMIx installation:

  .. code-block::

     inky:/opt/pmix-VERSION
     blinky:/opt/pmix-VERSION
     pinky:/opt/pmix-VERSION
     clyde:/opt/pmix-VERSION

  Notice that there are the same four directories as the previous
  example, but on ``inky``, the directory is *actually* located in
  ``/usr/local/pmix-VERSION``.

There is a bit of a disadvantage in this approach; each of the remote
nodes have to incur NFS (or whatever filesystem is used) delays to
access the PMIx directory tree.  However, both the administration
ease and low cost (relatively speaking) of using a networked file
system usually greatly outweighs the cost.

Local filesystem
^^^^^^^^^^^^^^^^

If you are concerned with networked filesystem costs of accessing the
PMIx binaries, you can install PMIx on the local hard drive of
each node in your system.  Again, it is *highly* advisable to install
PMIx in the *same* directory on each node so that each user's
``PATH`` can be set to the same value, regardless of the node that a
user has logged on to.

This approach will save some network latency of accessing the PMIx
binaries, but is typically only used where users are very concerned
about squeezing every single cycle out of their machines, or are
running at extreme scale where a networked filesystem may get
overwhelmed by filesystem requests for PMIx binaries when running
very large parallel jobs.

.. _building-pmix-install-overwrite-label:

Installing over a prior PMIx installation
-----------------------------------------

.. warning:: The PMIx team does not recommend installing a new
   version of PMIx over an existing / older installation of PMIx.

In its default configuration, an PMIx installation consists of
several shared libraries, header files, executables, and plugins
(dynamic shared objects |mdash| DSOs).  These installation files act
together as a single entity.  The specific filenames and
contents of these files are subject to change between different
versions of PMIx.

.. important:: Installing one version of PMIx does *not* uninstall
   another version.

If you install a new version of PMIx over an older version, this
may not overwrite all the files from the older version.  Hence, you
may end up with an incompatible muddle of files from two different
installations |mdash| which can cause problems.

The PMIx team recommends one of the following methods for
upgrading your PMIx installation:

* Install newer versions of PMIx into a different directory. For
  example, install into ``/opt/pmix-a.b.c`` and
  ``/opt/pmix-x.y.z`` for versions a.b.c and x.y.z, respectively.
* Completely uninstall the old version of PMIx before installing
  the new version.  The ``make uninstall`` process from PMIx a.b.c
  build tree should completely uninstall that version from the
  installation tree, making it safe to install a new version (e.g.,
  version x.y.z) into the same installation tree.
* Remove the old installation directory entirely and then install the
  new version.  For example ``rm -rf /opt/pmix`` *(assuming that
  there is nothing else of value in this tree!)* The installation of
  PMIx x.y.z will safely re-create the ``/opt/pmix`` tree.
  This method is preferable if you no longer have the source and build
  trees to PMIx a.b.c available from which to ``make
  uninstall``.
* Go into the PMIx a.b.c installation directory and manually
  remove all old PMIx files.  Then install PMIx x.y.z into the
  same installation directory.  This can be a somewhat painful,
  annoying, and error-prone process.  *We do not recommend it.*
  Indeed, if you no longer have access to the original PMIx a.b.c
  source and build trees, it may be far simpler to download PMIx
  version a.b.c again from the PMIx web site, configure it with
  the same installation prefix, and then run ``make uninstall``.  Or
  use one of the other methods, above.

Relocating an PMIx installation
-------------------------------

It can be desirable to initially install PMIx to one location
(e.g., ``/path/to/pmix``) and then later move it to another
location (e.g., ``/opt/myproduct/bundled-pmix-a.b.c``).

.. note:: PMIx hard-codes some directory paths in its executables
          based on installation paths specified by the ``configure``
          script.  For example, if you configure with an installation
          prefix of ``/opt/pmix/``, PMIx encodes in its
          executables that it should be able to find its help files in
          ``/opt/pmix/share/pmix``.

The "installdirs" functionality in PMIx lets you change any of
these hard-coded directory paths at run time (*assuming* that you have
already adjusted your ``PATH`` and/or ``LD_LIBRARY_PATH`` environment
variables to the new location where PMIx now resides).

There are three methods.

Move an existing PMIx installation to a new prefix
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Set the ``PMIX_PREFIX`` environment variable before launching a
PMIx-based application.  For example, if PMIx had initially been installed to
``/opt/pmix`` and the entire ``pmix`` tree was later moved to
``/home/pmix``, setting ``PMIX_PREFIX`` to ``/home/pmix`` will
enable PMIx to function properly.

"Stage" an PMIx installation in a temporary location
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When *creating* self-contained installation packages, systems such as
RPM install PMIx into temporary locations.  The package system
then bundles up everything under the temporary location into a package
that can be installed into its real location later.  For example, when
*creating* an RPM that will be installed to ``/opt/pmix``, the RPM
system will transparently prepend a "destination directory" (or
"destdir") to the installation directory.  As such, PMIx will
think that it is installed in ``/opt/pmix``, but it is actually
temporarily installed in (for example)
``/var/rpm/build.1234/opt/pmix``.  If it is necessary to *use* PMIx
while it is installed in this staging area, the ``PMIX_DESTDIR``
environment variable can be used; setting ``PMIX_DESTDIR`` to
``/var/rpm/build.1234`` will automatically prefix every directory such
that PMIx can function properly.

Overriding individual directories
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

PMIx uses the GNU-specified directories (per Autoconf/Automake),
and can be overridden by setting environment variables directly
related to their common names.  The list of environment variables that
can be used is:

* ``PMIX_PREFIX``
* ``PMIX_EXEC_PREFIX``
* ``PMIX_BINDIR``
* ``PMIX_SBINDIR``
* ``PMIX_LIBEXECDIR``
* ``PMIX_DATAROOTDIR``
* ``PMIX_DATADIR``
* ``PMIX_SYSCONFDIR``
* ``PMIX_SHAREDSTATEDIR``
* ``PMIX_LOCALSTATEDIR``
* ``PMIX_LIBDIR``
* ``PMIX_INCLUDEDIR``
* ``PMIX_INFODIR``
* ``PMIX_MANDIR``
* ``PMIX_PKGDATADIR``
* ``PMIX_PKGLIBDIR``
* ``PMIX_PKGINCLUDEDIR``

Note that not all of the directories listed above are used by PMIx;
they are listed here in entirety for completeness.

Also note that several directories listed above are defined in terms
of other directories.  For example, the ``$bindir`` is defined by
default as ``$prefix/bin``.  Hence, overriding the ``$prefix`` (via
``PMIX_PREFIX``) will automatically change the first part of the
``$bindir`` (which is how method 1 described above works).
Alternatively, ``PMIX_BINDIR`` can be set to an absolute value that
ignores ``$prefix`` altogether.
