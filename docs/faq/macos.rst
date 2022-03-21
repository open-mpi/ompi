MacOS
=====

.. TODO How can I create a TOC just for this page here at the top?

/////////////////////////////////////////////////////////////////////////

How does Open MPI handle HFS+ / UFS / AFP filesystems?
------------------------------------------------------

Generally, Open MPI does not care whether it is running from an Apple
filesystem.  However, the C++ wrapper compiler historically has been
called ``mpiCC``, which of course is the same file as ``mpicc`` when
running on a case-insensitive filesystem such as HFS+.

During the ``configure`` process, Open MPI will attempt to determine
if the build filesystem is case sensitive or not, and assume the
install file system is the same way.  Generally, this is all that is
needed to deal with HFS+.

You can force ``configure`` to build for a case-sensitive filesystem
by using the ``--with-cs-fs`` CLI option, or force ``configure`` to
build for a case insensitive filesystem by using ``--without-cs-fs``.

/////////////////////////////////////////////////////////////////////////

How do I use the Open MPI wrapper compilers in XCode?
-----------------------------------------------------

XCode has a non-public interface for adding compilers to XCode.  A
friendly Open MPI user sent in a configuration file for XCode 2.3
``MPICC.pbcompspec``, which will add support for the
Open MPI wrapper compilers.

Create a file named ``/Library/Application Support/Apple/Developer
Tools/Specifications/MPICC.pbcompspec`` in a text editor and put the
following content in it:

.. code-block::

   /**
    Xcode Compiler Specification for MPICC
   */

   {
      Type = Compiler;
      Identifier = com.apple.compilers.mpicc;
      BasedOn = com.apple.compilers.gcc.4_0;
      Name = "MPICC";
      Version = "Default";
      Description = "MPI GNU C/C++ Compiler 4.0";
      ExecPath = "/usr/local/bin/mpicc";      // This gets converted to the g++ variant automatically
      PrecompStyle = pch;
   }

Upon starting XCode, this file is loaded and added to the list of
known compilers.

.. warning:: This file has not been tested since XCode 2.3.  YMMV.

To use the ``mpicc`` compiler: open the project, get info on the
target, click the rules tab, and add a new entry.  Change the process
rule for "C source files" and select "using MPICC".

Before moving the file, the ``ExecPath`` parameter should be set
to the location of the Open MPI install.  The ``BasedOn`` parameter
should be updated to refer to the compiler version that ``mpicc``
will invoke &mdash; generally ``gcc-4.0`` on OS X 10.4 machines.

Thanks to Karl Dockendorf for this information.

/////////////////////////////////////////////////////////////////////////

How do I get Open MPI for MacOS?
--------------------------------

There are two main options for installing Open MPI on MacOS:

#. Use a package manager, such as `Homebrew <https://brew.sh/>`_ or
   `MacPorts <https://macports.org/>`_.  For example:

   .. code-block:: sh

      # For Homebrew
      shell$ brew install openmpi

      # For MacPorts
      shell$ port install openmpi

#. Install Open MPI from source.  :doc:`See the installation section
   of this documentation </installing-open-mpi/index>` for more details.

   .. warning:: Ensure to install a Fortran compiler if you want Open
                MPI to build the Fortran MPI interfaces.  For
                simplicity, the Open MPI team recommends using
                Homebrew or MacPorts to install a Fortran compiler.

/////////////////////////////////////////////////////////////////////////

I'm getting weird messages about filenames that are too long
------------------------------------------------------------

.. error:: TODO Find out specific information about how MacOS's tmpdir
           is very long, and macOS users may need to redefine it.
