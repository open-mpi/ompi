Installing PRRTE
================

For More Information
--------------------

This file is a *very* short overview of building and installing the
PMIx Reference RunTime Environment (PRRTE).  More information is
available `in the FAQ section on the PRRTE web site
<https://github.com/openpmix/prrte>`_.

Minimum PMIx version
--------------------

The ``configure`` script in PRRTE |prte_ver| must be able to find an
OpenPMIx installation that is |pmix_min_version| or higher.  If
``configure`` cannot find a suitable OpenPMIx version, it will abort
with an error.

If OpenPMIx cannot be found in default preprocessor and linker search
paths, you can specify the ``--with-pmix=DIR`` CLI option to tell
``configure`` where to find it.


Developer Builds
----------------

If you have checked out a DEVELOPER'S COPY of PRRTE (i.e., you checked
out from Git), you should read the :doc:`Developer's Guide
</developers/index>` section before attempting to build PRRTE.  You
must then run:

.. code-block:: sh

   shell$ ./autogen.pl

You will need very recent versions of GNU Autoconf, Automake, and
Libtool.  If ``autogen.pl`` fails, read the :doc:`Developer's Guide
</developers/index>`.  If anything else fails, read the
:doc:`Developer's Guide </developers/index>`.  Finally, we suggest
reading the :doc:`Developer's Guide </developers/index>`.

.. note:: Developer's copies of PRRTE typically include a large
          performance penalty at run-time because of extra debugging
          overhead.


User Builds
-----------

Building PRRTE is typically a combination of running ``configure``
and ``make``.  Execute the following commands to install the PRRTE
system from within the directory at the top of the tree:

.. code-block:: sh

   shell$ ./configure --prefix=/where/to/install
   [...lots of output...]
   shell$ make all install

If you need special access to install, then you can execute ``make
all`` as a user with write permissions in the build tree, and a
separate ``make install`` as a user with write permissions to the
install tree.

Compiling support for specific compilers and environments may require
additional command line flags when running ``configure``.  Note that
VPATH builds are fully supported.  For example:

.. code-block:: sh

   shell$ tar xf prrte-X.Y.Z.tar.gz
   shell$ cd prrte-X.Y.Z
   shell$ mkdir build
   shell$ cd build
   shell$ ../configure ...your options...
   [...lots of output...]
   shell$ make all install

Parallel builds are also supported (although some versions of ``make``,
such as GNU make, will only use the first target listed on the command
line when executable parallel builds).  For example (assume GNU make):

.. code-block:: sh

   shell$ make -j 4 all
   [...lots of output...]
   shell$ make install

Parallel make is generally only helpful in the build phase; the
installation process is mostly serial and does not benefit much from
parallel make.
