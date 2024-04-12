Definitions
===========

* **Source tree:** The tree where the PMIx source code is located.
  It is typically the result of expanding an PMIx distribution
  source code bundle, such as a tarball.
* **Build tree:** The tree where PMIx was built.  It is always
  related to a specific source tree, but may actually be a different
  tree (:ref:`since PMIx supports VPATH builds
  <building-pmix-vpath-label>`).  Specifically, this is the tree
  where you invoked ``configure``, ``make``, etc. to build and install
  PMIx.
* **Installation tree:** The tree where PMIx was installed.  It is
  typically the "prefix" argument given to PMIx's ``configure``
  script; it is the directory from which you run installed PMIx
  executables.
