.. _developers-installing-autotools-label:

Manually installing the GNU Autootools
======================================

There is enough detail in building the GNU Autotools that it warrants
its own section.

.. note:: As noted above, you only need to read/care about this
          section if you are building PMIx from a Git clone.  End
          users installing an PMIx distribution tarball do *not*
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

The following tools are required for developers to compile PMIx
from its repository sources (users who download PMIx tarballs do
not need these tools - they are only required for developers working
on the internals of PMIx itself):

.. list-table::
   :header-rows: 1

   * - Software package
     - Version
     - URL

   * - GNU m4
     - 1.4.17
     - https://ftp.gnu.org/gnu/m4/
   * - GNU Autoconf
     - 2.69
     - https://ftp.gnu.org/gnu/autoconf/
   * - GNU Automake
     - 1.15
     - https://ftp.gnu.org/gnu/automake/
   * - GNU Libtool
     - 2.4.6
     - https://ftp.gnu.org/gnu/libtool/
   * - Flex
     - 2.5.35
     - https://sourceforge.net/projects/flex/
   * - Sphinx
     - 4.2.0
     - https://www.sphinx-doc.org/en/master/index.html

The above table lists the versions that are used to make official
release PMIx tarballs. Other versions of the
tools *may* work for some (but almost certainly not all) platforms;
the ones listed below are the versions that we know work across an
extremely wide variety of platforms and environments.

To strengthen this point: the core PMIx developers typically
use very, very recent versions of the GNU tools.  There are known bugs
in older versions of the GNU tools that PMIx no longer compensates
for (it seemed senseless to indefinitely support patches for ancient
versions of Autoconf, for example).

.. warning:: You **will** have problems if you do not use recent
             versions of the GNU Autotools.

That being said, ``autogen.pl`` and ``configure.ac`` scripts tend to
be a bit lenient and enforce slightly older minimum versions than the
ones listed above. This is because such older versions still make
usable PMIx builds on many platforms - especially Linux on x86_64
with GNU compilers - and are convenient for developers whose Linux
distribution may not have as recent as the versions listed above (but are
recent enough to produce a working version for their platform).

To be clear: the versions listed above are required to support a wide
variety of platforms and environments, and are used to make the official
release tarballs. When building PMIx, YMMV when using
versions older than those listed above |mdash| especially if you are
not building on Linux x86_64 with the GNU compilers.

Using older versions is unsupported. If you run into problems, upgrade
to at least the versions listed above.

.. note:: Sphinx was only recently added to the list of required tools
          to generate the documentation. It is not required for PMIx
          versions prior to v4.2.3.

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
tools together.  But note that PMIx has no specific m4
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
