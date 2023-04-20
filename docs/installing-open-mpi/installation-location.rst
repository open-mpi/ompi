
.. _building-open-mpi-installation-location-label:

Installation location
=====================

A common environment to run Open MPI is in a "Beowulf"-class or
similar cluster (e.g., a bunch of 1U servers in a bunch of racks).
Simply stated, Open MPI can run on a group of servers or workstations
connected by a network.  As mentioned in the
:ref:`prerequisites section <running-prerequisites-label>` there are
several caveats however (for example, you typically must have an
account on all the machines, you can ``ssh`` between the
nodes without using a password, etc.).

Regardless of whether Open MPI is installed on a shared / networked
filesystem or independently on each node, it is usually easiest if
Open MPI is available in the same filesystem path on every node.
For example, if you install Open MPI to ``/opt/openmpi-|ompi_ver|`` on
one node, ensure that it is available in ``/opt/openmpi-|ompi_ver|``
on *all* nodes.

.. important:: For simplicity, the Open MPI team *strongly* recommends
   that you install Open MPI at the same path location on all nodes in
   your cluster.  This *greatly* simplifies the user experience of
   running MPI jobs across multiple nodes in your cluster.

   It is *possible* to install Open MPI in unique path locations in
   the different nodes in your cluster, but it is not *advisable*.

This raises the question for Open MPI system administrators: where to
install the Open MPI binaries, header files, etc.?  This discussion
mainly addresses this question for homogeneous clusters (i.e., where
all nodes and operating systems are the same), although elements of
this discussion apply to heterogeneous clusters as well.

Filesystem types
----------------

There are two common approaches.

Network filesystem
^^^^^^^^^^^^^^^^^^

Have a common filesystem, such as NFS, between all the machines to be
used.  Install Open MPI such that the installation directory is the
*same value* on each node.  This will *greatly* simplify user's shell
startup scripts (e.g., ``.bashrc``, ``.cshrc``, ``.profile`` etc.)
|mdash| the ``PATH`` can be set without checking which machine the
user is on.  It also simplifies the system administrator's job; when
the time comes to patch or otherwise upgrade Open MPI, only one copy
needs to be modified.

For example, consider a cluster of four machines: ``inky``,
``blinky``, ``pinky``, and ``clyde``.

* Install Open MPI on ``inky``'s local hard drive in the directory
  ``/opt/openmpi-VERSION``.  The system administrator then mounts
  ``inky:/opt/openmpi-VERSION`` on the remaining three machines, such
  that ``/opt/openmpi-VERSION`` on all machines is effectively "the
  same".  That is, the following directories all contain the Open MPI
  installation:

  .. code-block::

     inky:/opt/openmpi-VERSION
     blinky:/opt/openmpi-VERSION
     pinky:/opt/openmpi-VERSION
     clyde:/opt/openmpi-VERSION

* Install Open MPI on ``inky``'s local hard drive in the directory
  ``/usr/local/openmpi-VERSION``.  The system administrator then
  mounts ``inky:/usr/local/openmpi-VERSION`` on *all four* machines in
  some other common location, such as ``/opt/openmpi-VERSION`` (a
  symbolic link can be installed on ``inky`` instead of a mount point
  for efficiency).  This strategy is typically used for environments
  where one tree is NFS exported, but another tree is typically used
  for the location of actual installation.  For example, the following
  directories all contain the Open MPI installation:

  .. code-block::

     inky:/opt/openmpi-VERSION
     blinky:/opt/openmpi-VERSION
     pinky:/opt/openmpi-VERSION
      clyde:/opt/openmpi-VERSION

  Notice that there are the same four directories as the previous
  example, but on ``inky``, the directory is *actually* located in
  ``/usr/local/openmpi-VERSION``.

There is a bit of a disadvantage in this approach; each of the remote
nodes have to incur NFS (or whatever filesystem is used) delays to
access the Open MPI directory tree.  However, both the administration
ease and low cost (relatively speaking) of using a networked file
system usually greatly outweighs the cost.  Indeed, once an MPI
application is past MPI initialization, it doesn't use the Open MPI
binaries very much.

Local filesystem
^^^^^^^^^^^^^^^^

If you are concerned with networked filesystem costs of accessing the
Open MPI binaries, you can install Open MPI on the local hard drive of
each node in your system.  Again, it is *highly* advisable to install
Open MPI in the *same* directory on each node so that each user's
``PATH`` can be set to the same value, regardless of the node that a
user has logged on to.

This approach will save some network latency of accessing the Open MPI
binaries, but is typically only used where users are very concerned
about squeezing every single cycle out of their machines, or are
running at extreme scale where a networked filesystem may get
overwhelmed by filesystem requests for Open MPI binaries when running
very large parallel jobs.

.. _building-open-mpi-install-overwrite-label:

Installing over a prior Open MPI installation
---------------------------------------------

.. warning:: The Open MPI team does not recommend installing a new
   version of Open MPI over an existing / older installation of Open
   MPI.

In its default configuration, an Open MPI installation consists of
several shared libraries, header files, executables, and plugins
(dynamic shared objects |mdash| DSOs).  These installation files act
together as a single entity.  The specific filenames and
contents of these files are subject to change between different
versions of Open MPI.

.. important:: Installing one version of Open MPI does *not* uninstall
   another version.

If you install a new version of Open MPI over an older version, this
may not overwrite all the files from the older version.  Hence, you
may end up with an incompatible muddle of files from two different
installations |mdash| which can cause problems.

See :ref:`updating Open MPI <building-open-mpi-updating-label>` for more
information about updating or upgrading an installation of Open MPI.

Relocating an Open MPI installation
-----------------------------------

It can be desirable to initially install Open MPI to one location
(e.g., ``/path/to/openmpi``) and then later move it to another
location (e.g., ``/opt/myproduct/bundled-openmpi-a.b.c``).

.. note:: Open MPI hard-codes some directory paths in its executables
          based on installation paths specified by the ``configure``
          script.  For example, if you configure with an installation
          prefix of ``/opt/openmpi/``, Open MPI encodes in its
          executables that it should be able to find its help files in
          ``/opt/openmpi/share/openmpi``.

The "installdirs" functionality in Open MPI lets you change any of
these hard-coded directory paths at run time (*assuming* that you have
already adjusted your ``PATH`` and/or ``LD_LIBRARY_PATH`` environment
variables to the new location where Open MPI now resides).

There are three methods.

Move an existing Open MPI installation to a new prefix
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Set the ``OPAL_PREFIX`` environment variable before launching Open
MPI.  For example, if Open MPI had initially been installed to
``/opt/openmpi`` and the entire ``openmpi`` tree was later moved to
``/home/openmpi``, setting ``OPAL_PREFIX`` to ``/home/openmpi`` will
enable Open MPI to function properly.

"Stage" an Open MPI installation in a temporary location
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When *creating* self-contained installation packages, systems such as
RPM install Open MPI into temporary locations.  The package system
then bundles up everything under the temporary location into a package
that can be installed into its real location later.  For example, when
*creating* an RPM that will be installed to ``/opt/openmpi``, the RPM
system will transparently prepend a "destination directory" (or
"destdir") to the installation directory.  As such, Open MPI will
think that it is installed in ``/opt/openmpi``, but it is actually
temporarily installed in (for example)
``/var/rpm/build.1234/opt/openmpi``.  If it is necessary to *use* Open
MPI while it is installed in this staging area, the ``OPAL_DESTDIR``
environment variable can be used; setting ``OPAL_DESTDIR`` to
``/var/rpm/build.1234`` will automatically prefix every directory such
that Open MPI can function properly.

Overriding individual directories
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Open MPI uses the GNU-specified directories (per Autoconf/Automake),
and can be overridden by setting environment variables directly
related to their common names.  The list of environment variables that
can be used is:

* ``OPAL_PREFIX``
* ``OPAL_EXEC_PREFIX``
* ``OPAL_BINDIR``
* ``OPAL_SBINDIR``
* ``OPAL_LIBEXECDIR``
* ``OPAL_DATAROOTDIR``
* ``OPAL_DATADIR``
* ``OPAL_SYSCONFDIR``
* ``OPAL_SHAREDSTATEDIR``
* ``OPAL_LOCALSTATEDIR``
* ``OPAL_LIBDIR``
* ``OPAL_INCLUDEDIR``
* ``OPAL_INFODIR``
* ``OPAL_MANDIR``
* ``OPAL_PKGDATADIR``
* ``OPAL_PKGLIBDIR``
* ``OPAL_PKGINCLUDEDIR``

Note that not all of the directories listed above are used by Open
MPI; they are listed here in entirety for completeness.

Also note that several directories listed above are defined in terms
of other directories.  For example, the ``$bindir`` is defined by
default as ``$prefix/bin``.  Hence, overriding the ``$prefix`` (via
``OPAL_PREFIX``) will automatically change the first part of the
``$bindir`` (which is how method 1 described above works).
Alternatively, ``OPAL_BINDIR`` can be set to an absolute value that
ignores ``$prefix`` altogether.

.. _building-open-mpi-installation-location-multiple-copies-label:

Installing Multiple Copies of Open MPI
--------------------------------------

Open MPI can handle a variety of different run-time environments
(e.g., ssh, Slurm, PBS, etc.) and a variety of different
interconnection networks (e.g., ethernet, InfiniBand, etc.)
in a single installation.  Specifically: because Open MPI is
fundamentally powered by a component architecture, plug-ins for all
these different run-time systems and interconnect networks can be
installed in a single installation tree.  The relevant plug-ins will
only be used in the environments where they make sense.

Hence, there is no need to have one MPI installation for InfiniBand, one
MPI installation for ethernet, one MPI installation for PBS, one MPI
installation for ``ssh``, etc.  Open MPI can handle all of these in a
single installation.

However, there are some issues that Open MPI cannot solve.  Binary
compatibility between different compilers is such an issue and may require
installation of multiple versions of Open MPI.

Let's examine this on a per-language basis (be sure see the big caveat at
the end):

* *C:* Most C compilers are fairly compatible, such that if you compile
  Open MPI with one C library and link it to an application that was
  compiled with a different C compiler, everything should "just work."
  As such, a single installation of Open MPI should work for most C MPI
  applications.

* *C++:* The same is not necessarily true for C++.  While Open MPI does not
  currently contain any C++ code (the MPI C++ bindings were removed in a prior
  release), and C++ compilers *should* produce ABI-equivalent code for C
  symbols, obscure problem can sometimes arise when mixing compilers from
  different suites.  For example, if you compile Open MPI with the XYZ C/C++
  compiler, you may need to have the XYC C++ run-time libraries
  installed everywhere you want to run.

* *Fortran:* There are multiple issues with Fortran.

    #. Fortran compilers do something called "symbol mangling," meaning that the
       back-end symbols may have slightly different names than their corresponding
       global variables, subroutines, and functions.  There are 4 common name
       mangling schemes in use by Fortran compilers.  On many systems (e.g.,
       Linux), Open MPI will automatically support all 4 schemes.  As such, a
       single Open MPI installation *should* just work with multiple different
       Fortran compilers.  However, on some systems, this is not possible (e.g.,
       OS X), and Open MPI will only support the name mangling scheme of the
       Fortran compiler that was identified during ``configure``.

    #. That being said, there are two notable exceptions that do *not* work
       across Fortran compilers that are "different enough":

        #. The C constants ``MPI_F_STATUS_IGNORE`` and ``MPI_F_STATUSES_IGNORE``
             will only compare properly to Fortran applications that were
             created with Fortran compilers that that use the same
             name-mangling scheme as the Fortran compiler with which Open MPI was
             configured.

        #. Fortran compilers may have different values for the logical
             ``.TRUE.`` constant.  As such, any MPI function that uses the
             Fortran ``LOGICAL`` type may only get ``.TRUE.`` values back that
             correspond to the the ``.TRUE.`` value of the Fortran compiler with which
             Open MPI was configured.

    #. Similar to C++, linking object files that Fortran language features such
       as modules and/or polymorphism from different
       Fortran compilers is not likely to work.  The ``mpi`` and ``mpi_f08`` modules that
       Open MPI creates will likely only work with the Fortran compiler
       that was identified during ``configure`` (and used to build Open MPI).

The big caveat to all of this is that Open MPI will only work with
different compilers *if all the datatype sizes are the same.*  For
example, even though Open MPI supports all 4 name mangling schemes,
the size of the Fortran ``LOGICAL`` type may be 1 byte in some compilers
and 4 bytes in others.  This will likely cause Open MPI to perform
unpredictably.

The bottom line is that Open MPI can support all manner of run-time
systems and interconnects in a single installation, but supporting
multiple compilers "sort of" works (i.e., is subject to trial and
error) in some cases, and definitely does not work in other cases.
There's unfortunately little that we can do about this |mdash| it's a
compiler compatibility issue, and one that compiler authors have
little incentive to resolve.
