Definitions
===========

* **Source tree:** The tree where the Open MPI source code is located.
  It is typically the result of expanding an Open MPI distribution
  source code bundle, such as a tarball.
* **Build tree:** The tree where Open MPI was built.  It is always
  related to a specific source tree, but may actually be a different
  tree (:ref:`since Open MPI supports VPATH builds
  <building-open-mpi-vpath-label>`).  Specifically, this is the tree
  where you invoked ``configure``, ``make``, etc. to build and install
  Open MPI.
* **Installation tree:** The tree where Open MPI was installed.  It is
  typically the "prefix" argument given to Open MPI's ``configure``
  script; it is the directory from which you run installed Open MPI
  executables.
