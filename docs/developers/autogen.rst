Running ``autogen.pl``
======================

You can now run OMPI's top-level ``autogen.pl`` script.  This script
will invoke the GNU Autoconf, Automake, and Libtool commands in the
proper order and do a bunch of component discovery and housekeeping to
setup to run OMPI's top-level ``configure`` script.

Running ``autogen.pl`` may take a few minutes, depending on your
system.  It's not very exciting to watch.

If you have a multi-processor system, enabling the multi-threaded
behavior in Automake 1.11 (or newer) can result in ``autogen.pl``
running faster.  Do this by setting the ``AUTOMAKE_JOBS`` environment
variable to the number of processors (threads) that you want it to use
before invoking ``autogen.pl``.  For example (you can put this in your
shell startup files)::

   # For bash/sh/zsh:
   export AUTOMAKE_JOBS=4

   # For csh/tcsh:
   set AUTOMAKE_JOBS 4

.. important:: You generally need to run ``autogen.pl`` whenever the
   top-level file ``configure.ac`` changes, or any files in the
   ``config/`` or ``<project>/config/`` directories change (these
   directories are where a lot of "include" files for Open MPI's
   ``configure`` script live).

.. note:: You do *NOT* need to re-run ``autogen.pl`` if you modify a
   ``Makefile.am``.
