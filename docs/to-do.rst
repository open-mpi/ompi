To-Do Items
===========

This is a "to-do" file for while we are working on creating the first
version of the Open MPI RST / Sphinx docs.  *It will not be included in
the final documentation.*

Things that have changed in v5.0
--------------------------------

Need to update these docs to reflect:

* What specifically does ``--disable-io-romio`` do?

* Do we still have AMCA files?

* Do we still have ``--tune`` files?

* Document this new breakpoint functionality:
  https://github.com/open-mpi/ompi/commit/f97d081cf9b540c5a79e00aecee17b25e8c123ad

* Add that we *do* support "single dash" ``mpirun`` options, but we
  don't want people to do that.

* Document v5.0.x's backwards-compatibility relationship with v4.x
  (including ABI and ``mpirun`` command line options).  It will likely
  be:

  * C bindings are ABI compatible
  * Fortran ``mpif.h`` *should* be ok (but probably bears checking).
  * Fortran module ABIs may have changed.  These definitely need
    checking (e.g., w.r.t. ``MPI_Status`` and friends...?).
  * MPI-1 deleted functions are always included in the library

    * Per Brian: the behavior is the same as Open MPI 4.x. The deleted
      functions are not declared in mpi.h unless Open MPI was compiled
      with ``--enable-mpi1-compatibility``.

* This is not really a change in v5.0, but it probably has changed
  (because of PRTE) since v4.x, so: we need to document how to use
  ``MPI_Comm_join`` and ``MPI_Publish_name`` + ``MPI_Comm_accept`` /
  ``MPI_Lookup_name`` + ``MPI_Comm_connect``.  See
  https://github.com/open-mpi/ompi/issues/10222

Other random to-do items
------------------------

* Add a note somewhere about "These HTML docs are available at
  docs.open-mpi.org and in distribution tarballs in
  ``docs/_build/html/index.html``.

* Add a section about debugging and removal of MPIR, yadda yadda yadda
  (at least some of this can come from the "parallel debugging" FAQ
  section).

* Can we make a new ".. knownissues::" directive (etc.) for the NEWS
  file that does the same thing as ".. attention::", but says "Known
  Issues" instead of "Attention".

  Reading the Python docs style guide, it kinda implies we can do that
  kind of thing...?

* Make "setting an MCA param" docs prominent in the doc (this already
  exists somewhere, but we need to make it prominent).

* https://github.com/open-mpi/ompi/issues/7668 (ORTE --> PRRTE
  user-visible changes)

  * Add docs about mpirun launch vs. direct launch.

* Finish folding in all FAQ topics into the main document.

Man page to-dos
---------------

* ``man-openmpi/man1/mpirun.1.rst`` is currently essentially an
  RST-ified version of OMPI v4.1's ``mpirun(1)`` man page.  There has
  been some light editing:

  * Eliminiated all ``-foo`` options
  * Modernized *some* of the examples

  Much more work needs to be done to update it for all the PRTE
  changes since Open MPI v4.1.x.

* It's possible that a lot of *tokens* in the RST man pages should be
  ``tokens``.

* Look for "See the ___ section" text and create appropriate cross
  references.

* There's some shmem man pages with list that have incorrect
  indentation in the RST, which results in odd line break, extra bold
  face, and strange indentation.  Example:
  man-openshmem/man3/shmem_double_prod_to_all.3.html -- look for the
  description of "target".

* It looks like much of the cross-linking that we have in the MPI
  man pages (e.g., when one MPI API is mentioned on a page, it
  automatically links to the man page for that API) doesn't exist in
  the OSHMEM pages.

* Some of our code blocks have line numbers, others do not.  I think
  I prefer to have the line numbers, but don't feel too strongly
  about it.

* Ensure somewhere that it is documented -- probably in the networking
  section? -- that it is necessary for resource manager daemons to
  have their /etc/security limits set properly for locked memory.

Josh Hursey notes
-----------------

Running MPI Applications Notes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reviewing:
  https://ompi--8329.org.readthedocs.build/en/8329/faq/running-mpi-apps.html

10.8.21
 - Is aggressive mode really determined by the slot count provided by PRRTE? Or is it determined
   by a query to hwloc with a reference ot the number of processes per node. It just surprises
   me that this part of OMPI is controlled by PRRTE instead of something more generic that might
   work with, say, Slurm direct launch via srun.
