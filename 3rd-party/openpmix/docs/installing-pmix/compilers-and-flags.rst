.. _install-configure-compilers-and-flags-label:

Specifying compilers and flags
==============================

Changing the compilers that PMIx uses to build itself uses the
standard GNU Autoconf mechanism of setting special environment variables
either before invoking ``configure`` or on the ``configure`` command
line itself.

The following environment variables are recognized by ``configure``:

* ``CC``: C compiler to use
* ``CFLAGS``: Compile flags to pass to the C compiler
* ``CPPFLAGS``: Preprocessor flags to pass to the C compiler
* ``LDFLAGS``: Linker flags to pass to all compilers
* ``LIBS``: Libraries to pass to all compilers (it is rarely
  necessary for users to need to specify additional ``LIBS``)
* ``PKG_CONFIG``: Path to the ``pkg-config`` utility

.. note:: PMIx |opmix_ver| does not contain any C++ code. However,
    you can use a C++ compiler to build PMIx if you so choose.

For example, to build with a specific instance of ``gcc``:

.. code-block:: sh

   shell$ ./configure \
       CC=/opt/gcc-a.b.c/bin/gcc CFLAGS=-m64 ...

.. note:: The PMIx community generally suggests using the above
   command line form for setting different compilers (vs. setting
   environment variables and then invoking ``./configure``).  The
   above form will save all variables and values in the ``config.log``
   file, which makes post-mortem analysis easier if problems occur.

.. warning:: Note that setting ``CFLAGS`` (etc.) does *not* affect the
             flags used by the :ref:`wrapper compiler
             <label-quickstart-building-apps>`.  In the above, example, you may
             also need to add ``-m64`` to the ``--with-wrapper-cflags``
             option:

             .. code-block::

                shell$ ./configure CFLAGS=-m64 \
                     --with-wrapper-cflags=-m64 ...

             Failure to do this will result in PMIx-based applications
             failing to compile / link properly.

             See the :ref:`Customizing wrapper compiler behavior
             <label-customizing-wrapper-compiler>` section for more
             details.

Note that if you intend to compile PMIx with a ``make`` other than
the default one in your ``PATH``, then you must either set the ``$MAKE``
environment variable before invoking PMIx's ``configure`` script, or
pass ``MAKE=your_make_prog`` to configure.  For example:

.. code-block:: sh

   shell$ ./configure MAKE=/path/to/my/make ...

This could be the case, for instance, if you have a shell alias for
``make``, or you always type ``gmake`` out of habit.  Failure to tell
``configure`` which non-default ``make`` you will use to compile PMIx
can result in undefined behavior (meaning: don't do that).

Note that you may also want to ensure that the value of
``LD_LIBRARY_PATH`` is set appropriately (or not at all) for your build
(or whatever environment variable is relevant for your operating
system).  For example, some users have been tripped up by setting to
use a non-default C compiler via the ``CC`` environment variable,
but then failing to set ``LD_LIBRARY_PATH`` to include the directory
containing that non-default C compiler's support libraries.
This causes PMIx's ``configure`` script to fail when it tries to
compile / link / run simple C programs.
