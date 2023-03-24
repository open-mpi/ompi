
.. _building-open-mpi-updating-label:

Updating or upgrading an Open MPI installation
==============================================

The Open MPI team recommends one of the following methods for
upgrading your Open MPI installation:

* Install newer versions of Open MPI into a different directory. For
  example, install into ``/opt/openmpi-a.b.c`` and
  ``/opt/openmpi-x.y.z`` for versions a.b.c and x.y.z, respectively.
* Completely uninstall the old version of Open MPI before installing
  the new version.  The ``make uninstall`` process from Open MPI a.b.c
  build tree should completely uninstall that version from the
  installation tree, making it safe to install a new version (e.g.,
  version x.y.z) into the same installation tree.
* Remove the old installation directory entirely and then install the
  new version.  For example ``rm -rf /opt/openmpi`` *(assuming that
  there is nothing else of value in this tree!)* The installation of
  Open MPI x.y.z will safely re-create the ``/opt/openmpi`` tree.
  This method is preferable if you no longer have the source and build
  trees to Open MPI a.b.c available from which to ``make
  uninstall``.
* Go into the Open MPI a.b.c installation directory and manually
  remove all old Open MPI files.  Then install Open MPI x.y.z into the
  same installation directory.  This can be a somewhat painful,
  annoying, and error-prone process.  *We do not recommend it.*
  Indeed, if you no longer have access to the original Open MPI a.b.c
  source and build trees, it may be far simpler to download Open MPI
  version a.b.c again from the Open MPI web site, configure it with
  the same installation prefix, and then run ``make uninstall``.  Or
  use one of the other methods, above.

Depending on the version of Open MPI that applications were initially compiled
against and the target version of Open MPI to which you upgraded, users may
need to recompile their applications.
See the :doc:`section on Open MPI's version numbering scheme </version-numbering>`
for more information.
