Prerequisites
=============

Compilers
---------

Although it should probably be assumed, you'll need a C compiler that
supports C99.

You'll also need a Fortran compiler if you want to build the Fortran
MPI bindings (the more recent the Fortran compiler, the better), and a
Java compiler if you want to build the (unofficial) Java MPI bindings.

GNU Autotools
-------------

When building Open MPI from its repository sources, the GNU Autotools
must be installed (i.e., `GNU Autoconf
<https://www.gnu.org/software/autoconf/>`_, `GNU Automake
<https://www.gnu.org/software/automake/>`_, and `GNU Libtool
<https://www.gnu.org/software/libtool/>`_).

.. list-table::
   :header-rows: 1
   :widths: 10 10

   * - Tool
     - Minimum version
   * - Autoconf
     - |autoconf_min_version|
   * - Automake
     - |automake_min_version|
   * - Libtool
     - |libtool_min_version|

.. note:: The GNU Autotools are *not* required when building Open MPI
          from distribution tarballs.  Open MPI distribution tarballs
          are bootstrapped such that end-users do not need to have the
          GNU Autotools installed.

You can generally install GNU Autoconf, Automake, and Libtool via your
Linux distribution native package system, or via Homebrew or MacPorts
on MacOS.  This usually "just works."

If you run into problems with the GNU Autotools, or need to download /
build them manually, see the :ref:`how to build and install GNU
Autotools section <developers-installing-autotools-label>` for much
more detail.

Perl
----

Open MPI still uses Perl for a few of its build scripts (most notably,
``autogen.pl``).

Generally speaking, any recent-ish release of Perl 5 should be
sufficient to correctly execute Open MPI's Perl scripts.

Flex
----

Minimum supported version: |flex_min_version|.

`Flex <https://github.com/westes/flex>`_ is used during the
compilation of a developer's checkout (it is not used to build
official distribution tarballs).  Other flavors of lex are *not*
supported: given the choice of making parsing code portable between
all flavors of lex and doing more interesting work on Open MPI, we
greatly prefer the latter.

Note that no testing has been performed to see what the minimum
version of Flex is required by Open MPI.  We suggest that you use
v2.5.35 at the earliest.

For now, Open MPI will allow developer builds with Flex |flex_min_version|.  This
is primarily motivated by the fact that RedHat/CentOS 5 ships with
Flex 2.5.4.  It is likely that someday Open MPI developer builds will
require Flex version >=2.5.35.

Note that the ``flex``-generated code generates some compiler warnings
on some platforms, but the warnings do not seem to be consistent or
uniform on all platforms, compilers, and flex versions.  As such, we
have done little to try to remove those warnings.

If you do not have Flex installed and cannot easily install it via
your operating system's packaging system (to include Homebrew or
MacPorts on MacOS), see `the Flex Github repository
<https://github.com/westes/flex>`_.


Sphinx (and therefore Python)
-----------------------------

`Sphinx <https://www.sphinx-doc.org/>`_ is a Python-based tool used to
generate both the HTML version of the documentation (that you are
reading right now) and the nroff man pages.

Official Open MPI distribution tarballs contain pre-built HTML
documentation and man pages.  This means that |mdash| similar to the GNU
Autotools |mdash| end users do not need to have Sphinx installed, but will
still have both the HTML documentation and man pages installed as part
of the normal configure / build / install process.

However, the HTML documentation and man pages are *not* stored in Open
MPI's Git repository; only the ReStructred Text source code of the
documentation is in the Git repository.  Hence, if you are building
Open MPI from a Git clone, you will need Sphinx (and some Python
modules) in order to build the HTML documentation and man pages.

.. important:: Most systems do not have Sphinx and/or the required
               Python modules installed by default.  :ref:`See the
               Installing Sphinx section
               <developers-installing-sphinx-label>` for details on
               how to install Sphinx and the required Python modules.

If ``configure`` is able to find Sphinx and the required Python
modules, it will automatically generate the HTML documentation and man
pages during the normal build procedure (i.e., during ``make all``).
If ``configure`` is *not* able to find Sphinx and/or the required
Python modules, it will simply skip building the documentation.

.. note:: If you have built/installed Open MPI from a Git clone and
          unexpectedly did not have the man pages installed, it is
          likely that you do not have Sphinx and/or the required
          Python modules available.

          :ref:`See the Installing Sphinx section
          <developers-installing-sphinx-label>` for details on how
          to install Sphinx and the required Python modules.

.. important:: ``make dist`` will fail if ``configure`` did not find
               Sphinx and/or the required Python modules.
               Specifically: if ``make dist`` is not able to generate
               the most up-to-date HTML documentation and man pages,
               you cannot build a distribution tarball.  **This is an
               intentional design decision.**
