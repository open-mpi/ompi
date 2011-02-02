2 Feb 2011

Description
===========

This sample "tcp2" BTL component is a simple example of how to build
an Open MPI MCA component from outside of the Open MPI source tree.
This is a valuable technique for 3rd parties who want to provide their
own components for Open MPI, but do not want to be in the mainstream
distribution (i.e., their code is not part of the main Open MPI code
base).

NOTE: We do recommend that 3rd party developers investigate using a
      DVCS such as Mercurial or Git to keep up with Open MPI
      development.  Using a DVCS allows you to host your component in
      your own copy of the Open MPI source tree, and yet still keep up
      with development changes, stable releases, etc.

Previous colloquial knowledge held that building a component from
outside of the Open MPI source tree required configuring Open MPI
--with-devel-headers, and then building and installing it.  This
configure switch installs all of OMPI's internal .h files under
$prefix/include/openmpi, and therefore allows 3rd party code to be
compiled outside of the Open MPI tree.

This method definitely works, but is annoying:

 * You have to ask users to use this special configure switch.
 * Not all users install from source; many get binary packages (e.g.,
   RPMs).

This example package shows two ways to build an Open MPI MCA component
from outside the Open MPI source tree:

 1. Using the above --with-devel-headers technique
 2. Compiling against the Open MPI source tree itself (vs. the
    installation tree)

The user still has to have a source tree, but at least they don't have
to be required to use --with-devel-headers (which most users don't) --
they can likely build off the source tree that they already used.

Example project contents
========================

The "tcp2" component is a direct copy of the TCP BTL as of January
2011 -- it has just been renamed so that it can be built separately
and installed alongside the real TCP BTL component.

Most of the mojo for both methods is handled in the example
components' configure.ac, but the same techniques are applicable
outside of the GNU Auto toolchain.

This sample "tcp2" component has an autogen.sh script that requires
the normal Autoconf, Automake, and Libtool.  It also adds the
following two configure switches:

 --with-openmpi-install=DIR

    If provided, DIR is an Open MPI installation tree that was
    installed --with-devel-headers.

    This switch uses the installed mpicc --showme:<foo> functionality
    to extract the relevant CPPFLAGS, LDFLAGS, and LIBS.

 --with-openmpi-source=DIR

    If provided, DIR is the source of a configured and built Open MPI
    source tree (corresponding to the version expected by the example
    component).  The source tree is not required to have been
    configured --with-devel-headers.

    This switch uses the source tree's config.status script to extract
    the relevant CPPFLAGS and CFLAGS.

Either one of these two switches must be provided, or appropriate
CPPFLAGS, CFLAGS, LDFLAGS, and/or LIBS must be provided such that
valid Open MPI header and library files can be found and compiled /
linked against, respectively.

Example use
===========

First, download, build, and install Open MPI:

-----
$ cd $HOME
$ wget \
  http://www.open-mpi.org/software/ompi/vX.Y/downloads/openmpi-X.Y.Z.tar.bz2
  [lots of output]
$ tar jxf openmpi-X.Y.Z.tar.bz2
$ cd openmpi-X.Y.Z
$ ./configure --prefix=/opt/openmpi ...
  [lots of output]
$ make -j 4 install
  [lots of output]
$ /opt/openmpi/bin/ompi_info | grep btl
                 MCA btl: self (MCA vA.B, API vM.N, Component vX.Y.Z)
                 MCA btl: sm (MCA vA.B, API vM.N, Component vX.Y.Z)
                 MCA btl: tcp (MCA vA.B, API vM.N, Component vX.Y.Z)
  [where X.Y.Z, A.B, and M.N are appropriate for your version of Open MPI]
$
-----

Notice the installed BTLs from ompi_info.

Now cd into this example project and build it, pointing it to the
source directory of the Open MPI that you just built.  Note that we
use the same --prefix as when installing Open MPI (so that the built
component will be installed into the Right place):

-----
$ cd /path/to/this/sample
$ ./autogen.sh
$ ./configure --prefix=/opt/openmpi --with-openmpi-source=$HOME/openmpi-X.Y.Z
  [lots of output]
$ make -j 4 install
  [lots of output]
$ /opt/openmpi/bin/ompi_info | grep btl
                 MCA btl: self (MCA vA.B, API vM.N, Component vX.Y.Z)
                 MCA btl: sm (MCA vA.B, API vM.N, Component vX.Y.Z)
                 MCA btl: tcp (MCA vA.B, API vM.N, Component vX.Y.Z)
                 MCA btl: tcp2 (MCA vA.B, API vM.N, Component vX.Y.Z)
  [where X.Y.Z, A.B, and M.N are appropriate for your version of Open MPI]
$
-----

Notice that the "tcp2" BTL is now installed.

Random notes
============

The component in this project is just an example; I whipped it up in
the span of several hours.  Your component may be a bit more complex
than this or have slightly different requirements.  So you may need to
tweak the configury or build system in each of the components to fit
what you need.

Changes required to the component to make it build in a standalone
mode:

1. Write your own configure script.  This component is just a sample.
   You basically need to build against an OMPI install that was
   installed --with-devel-headers or a built OMPI source tree.  See
   ./configure --help for details.

2. I also provided a bogus btl_tcp2_config.h (generated by configure).
   This file is not included anywhere, but it does provide protection
   against re-defined PACKAGE_* macros when running configure, which
   is quite annoying.

3. Modify Makefile.am to only build DSOs.  I.e., you can optionally
   take the static option out since the component can *only* build in
   DSO mode when building standalone.  That being said, it doesn't
   hurt to leave the static builds in -- this would (hypothetically)
   allow the component to be built both in-tree and out-of-tree.

Ping the Open MPI devel list if you have questions about this
project.

Enjoy.

- Jeff Squyres
