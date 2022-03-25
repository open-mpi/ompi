.. _developers-installing-autotools-label:

Manually installing the GNU Autootools
======================================

There is enough detail in building the GNU Autotools that it warrants
its own section.

.. note:: As noted above, you only need to read/care about this
          section if you are building Open MPI from a Git clone.  End
          users installing an Open MPI distribution tarball do *not*
          need to have the GNU Autotools installed.

Use a package manager
---------------------

It is recommended that you use your Linux distribution's package
manager, or Homebrew or MacPorts on macOS to install recent versions
of GNU Autoconf, Automake, and Libtool.

If you cannot, keep reading in this section to see how to build and
install these applications manually (i.e., download the source from
the internet and build/install it yourself).

Autotools versions
------------------

The following tools are required for developers to compile Open MPI
from its repository sources (users who download Open MPI tarballs do
not need these tools - they are only required for developers working
on the internals of Open MPI itself):

.. list-table::
   :header-rows: 1

   * - Software package
     - Notes
     - URL

   * - GNU m4
     - See version chart below
     - https://ftp.gnu.org/gnu/m4/
   * - GNU Autoconf
     - See version chart below
     - https://ftp.gnu.org/gnu/autoconf/
   * - GNU Automake
     - See version chart below
     - https://ftp.gnu.org/gnu/automake/
   * - GNU Libtool
     - See version chart below
     - https://ftp.gnu.org/gnu/libtool/

The table below lists the versions that are used to make nightly
snapshot and official release Open MPI tarballs. Other versions of the
tools *may* work for some (but almost certainly not all) platforms;
the ones listed below are the versions that we know work across an
extremely wide variety of platforms and environments.

To strengthen the above point: the core Open MPI developers typically
use very, very recent versions of the GNU tools.  There are known bugs
in older versions of the GNU tools that Open MPI no longer compensates
for (it seemed senseless to indefinitely support patches for ancient
versions of Autoconf, for example).

.. warning:: You **will** have problems if you do not use recent
             versions of the GNU Autotools.

That being said, ``autogen.pl`` and ``configure.ac`` scripts tend to
be a bit lenient and enforce slightly older minimum versions than the
ones listed below. This is because such older versions still make
usable Open MPI builds on many platforms - especially Linux on x86_64
with GNU compilers - and are convenient for developers whose Linux
distribution may not have as recent as the versions listed below (but are
recent enough to produce a working version for their platform).

To be clear: the versions listed below are required to support a wide
variety of platforms and environments, and are used to make nightly
and official release tarballs. When building Open MPI, YMMV when using
versions older than those listed below |mdash| especially if you are
not building on Linux x86_64 with the GNU compilers.

Using older versions is unsupported. If you run into problems, upgrade
to at least the versions listed below.

.. note:: You may need to scroll right in the following table.

.. list-table::
   :header-rows: 1

   * - Open MPI
     - M4
     - Autoconf
     - Automake
     - Libtool
     - Flex
     - Sphinx

   * - v1.0.x
     - NA
     - 2.58 - 2.59
     - 1.7 - 1.9.6
     - 1.5.16 - 1.5.22
     - 2.5.4
     - NA
   * - v1.1.x
     - NA
     - 2.59
     - 1.9.6
     - 1.5.16 - 1.5.22
     - 2.5.4
     - NA
   * - v1.2.x
     - NA
     - 2.59
     - 1.9.6
     - 1.5.22 - 2.1a
     - 2.5.4
     - NA
   * - v1.3.x
     - 1.4.11
     - 2.63
     - 1.10.1
     - 2.2.6b
     - 2.5.4
     - NA
   * - v1.4.x
     - 1.4.11
     - 2.63
     - 1.10.3
     - 2.2.6b
     - 2.5.4
     - NA
   * - v1.5.x for x=0-4
     - 1.4.13
     - 2.65
     - 1.11.1
     - 2.2.6b
     - 2.5.4
     - NA
   * - v1.5.x for x>=5
     - 1.4.16
     - 2.68
     - 1.11.3
     - 2.4.2
     - 2.5.35
     - NA
   * - v1.6.x
     - 1.4.16
     - 2.68
     - 1.11.3
     - 2.4.2
     - 2.5.35
     - NA
   * - v1.7.x
     - 1.4.16
     - 2.69
     - 1.12.2
     - 2.4.2
     - 2.5.35
     - NA
   * - v1.8.x
     - 1.4.16
     - 2.69
     - 1.12.2
     - 2.4.2
     - 2.5.35
     - NA
   * - v1.10.x
     - 1.4.16
     - 2.69
     - 1.12.2
     - 2.4.2
     - 2.5.35
     - NA
   * - v2.0.x through v4.y
     - 1.4.17
     - 2.69
     - 1.15
     - 2.4.6
     - 2.5.35
     - NA
   * - v5.0.x
     - 1.4.17
     - 2.69
     - 1.15
     - 2.4.6
     - 2.5.35
     - 4.2.0
   * - Git main
     - 1.4.17
     - 2.69
     - 1.15
     - 2.4.6
     - 2.5.35
     - 4.2.0

Checking your versions
----------------------

You can check what versions of the Autotools you have installed with
the following:

.. code-block:: sh

   shell$ m4 --version
   shell$ autoconf --version
   shell$ automake --version
   shell$ libtoolize --version

Installing the GNU Autotools from source
----------------------------------------

.. note:: Most operating system packaging systems (to include Homebrew
          and MacPorts on MacOS) install recent-enough versions of the
          GNU Autotools.  You should generally only install the GNU
          Autotools manually if you can't use your operating system
          packaging system to install them for you.

The GNU Autotools sources can be can be downloaded from:

* https://ftp.gnu.org/gnu/autoconf/
* https://ftp.gnu.org/gnu/automake/
* https://ftp.gnu.org/gnu/libtool/
* And if you need it: https://ftp.gnu.org/gnu/m4/

It is certainly easiest to download/build/install all four of these
tools together.  But note that Open MPI has no specific m4
requirements; it is only listed here because Autoconf requires minimum
versions of GNU m4.  Hence, you may or may not *need* to actually
install a new version of GNU m4.  That being said, if you are confused
or don't know, just install the latest GNU m4 with the rest of the GNU
Autotools and everything will work out fine.


Build and install ordering
--------------------------

You must build and install the GNU Autotools in the following order:

#. m4
#. Autoconf
#. Automake
#. Libtool

.. important:: You *must* install the last three tools (Autoconf,
               Automake, Libtool) into the same prefix directory.
               These three tools are somewhat inter-related, and if
               they're going to be used together, they *must* share a
               common installation prefix.

You can install m4 anywhere as long as it can be found in the path;
it may be convenient to install it in the same prefix as the other
three.  Or you can use any recent-enough m4 that is in your path.

.. warning:: It is *strongly* encouraged that you do **not** install
   your new versions over the OS-installed versions.  This could cause
   other things on your system to break.  Instead, install into
   ``$HOME/local``, or ``/usr/local``, or wherever else you tend to
   install "local" kinds of software.

   In doing so, be sure to prefix your ``$PATH`` with the directory
   where they are installed.  For example, if you install into
   ``$HOME/local``, you may want to edit your shell startup file
   (``.bashrc``, ``.cshrc``, ``.tcshrc``, etc.) to have something
   like

   .. code-block:: sh

      # For bash/sh:
      export PATH=$HOME/local/bin:$PATH
      # For csh/tcsh:
      set path = ($HOME/local/bin $path)

   Ensure to set your ``$PATH`` *before* you configure/build/install
   the four packages.

All four packages require two simple commands to build and
install:

.. code-block:: sh

   shell$ cd M4_DIRECTORY
   shell$ ./configure --prefix=PREFIX
   shell$ make all install

.. important:: If you are using a shell that does not automatically
               re-index the ``$PATH`` (e.g., the ``csh`` or ``tcsh``
               shells), be sure to run the ``rehash`` command before
               you install the next package so that the executables
               that were just installed can be found by the next
               package.

.. code-block:: sh

   # Make $PATH be re-indexed if necessary, e.g., via "rehash"
   shell$ cd AUTOCONF_DIRECTORY
   shell$ ./configure --prefix=PREFIX
   shell$ make all install

.. code-block:: sh

   # Make $PATH be re-indexed if necessary, e.g., via "rehash"
   shell$ cd AUTOMAKE_DIRECTORY
   shell$ ./configure --prefix=PREFIX
   shell$ make all install

.. code-block:: sh

   # Make $PATH be re-indexed if necessary, e.g., via "rehash"
   shell$ cd LIBTOOL_DIRECTORY
   shell$ ./configure --prefix=PREFIX
   shell$ make all install
