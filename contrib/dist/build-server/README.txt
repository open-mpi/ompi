This is a collection of scripts that we use on the OMPI build server
to make official and nightly tarballs.  I'm putting them in SVN
because it occurs to me that these scripts *only* live on the local
disk of eddie.osl.iu.edu; if that disk should ever fail, we'd be
totally hosed!

I unfortunately don't have time to fully document all of these scripts
at the moment, so this brief README will have to do for now.

- install-autotools-set.sh: a script to install a new set of (GNU
  autotools + Flex) in $HOME/local.  There's many manual steps here,
  and every time we do it by hand, we screw it up.  Hence, this script
  automates the whole process.

  Note that this script also creates a modulefile in
  $HOME/modules/autotools corresponding to the set of tools that was
  just adjusted.

  Modulefiles are assumed by several of the scripts below; you need to
  go to $HOME/modules/autotools and make a sym link from
  "ompi-<branch_name>" to the modulefile you want (e.g., ompi-trunk,
  ompi-1.7, hwloc-1.5, ...etc.).  The scripts below do something like
  this:

  module use $HOME/modules
  module load autotools/ompi-$branchname
  ./autogen.pl
  # ...etc.

- crontab.txt: the cron jobs that are running as "mpiteam" on
  eddie.osl.iu.edu as of 8 Aug 2012.  They show the CLI options to
  several of these scripts.

- openmpi-nightly-tarball.sh: script used to make the nightly tarballs
  and copy them to the live web tree

- openmpi-update-www.open-mpi.org.sh: this is the script fired by
  cron to basically run "svn up" on the live www.open-mpi.org web
  site, in local directory /l/osl/www/www.open-mpi.org (this is a
  network mount, actually)

- openmpi-test-tarball.sh: I don't remember what this is.  I suspect
  it isn't used anymore.

- openmpi-release.sh: use this script to make official Open MPI
  tarball releases.  Give it the path in the OMPI SVN repo to make the
  tarball from.  For example:

     ./openmpi-release trunk
     or
     ./openmpi-release branches/v1.6

- hwloc-nightly-tarball.sh: just like openmpi-nightly-tarball.sh, but
  for hwloc.

- hwloc-release.sh: just like openmpi-release.sh, but for hwloc.

- remove-old.pl: when a nightly tarball fails, the script leaves the
  broken tree around for a human to examine.  This script prunes any
  of these old directories.  It's set to fire by
  openmpi-nightly-tarball.sh, and removes any busted nightly build
  directories older than 28 days.

- openmpi-nightly-test-build.sh: I don't remember what this is.  I
  suspect it isn't used anymore.

- openmpi-nightly-tmpbranch-tarball.sh: I don't remember what this is.
  I suspect it isn't used anymore.

- openmpi-ft-cr-tarball.sh: I suspect this was used by Josh to make
  tarballs of his FT work when it was off on a branch.  I suspect it
  isn't used anymore.
