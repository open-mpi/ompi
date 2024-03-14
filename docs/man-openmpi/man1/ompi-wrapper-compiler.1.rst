.. _man1-mpicc:
.. _man1-mpic++:
.. _man1-mpicxx:
.. _man1-mpifort:
.. _man1-mpijavac:

Open MPI Wrapper Compilers
==========================

.. include_body

mpicc, mpic++, mpicxx, mpifort, mpijavac |mdash| Open MPI wrapper compilers

SYNTAX
------

``mpicc [--showme | --showme:compile | --showme:link] ...``

``mpic++ [--showme | --showme:compile | --showme:link] ...``

``mpicxx [--showme | --showme:compile | --showme:link] ...``

``mpifort [--showme | --showme:compile | --showme:link] ...``

``mpijavac [--showme | --showme:compile | --showme:link] ...``

The following deprecated commands are also available |mdash| but
``mpifort`` should be used instead:

``mpif77 [--showme | --showme:compile | --showme:link] ...``

``mpif90 [--showme | --showme:compile | --showme:link] ...``

On case-sensitive filesystems, the following command will also be
available:

``mpiCC [--showme | --showme:compile | --showme:link] ...``


OPTIONS
-------

The options below apply to all of the wrapper compilers:

* ``--showme``: This option comes in several different variants (see
  below). None of the variants invokes the underlying compiler; they
  all provide information on how the underlying compiler would have
  been invoked had ``--showme`` not been used. The basic ``--showme``
  option outputs the command line that would be executed to compile
  the program.

  .. note:: If a non-filename argument is passed on the command line,
            the ``--showme`` option will *not* display any additional
            flags. For example, both ``"mpicc --showme`` and
            ``mpicc --showme my_source.c`` will show all the
            wrapper-supplied flags. But ``mpicc
            --showme -v`` will only show the underlying compiler name
            and ``-v``.

* ``--showme:compile``: Output the compiler flags that would have been
  supplied to the underlying compiler.

* ``--showme:link``: Output the linker flags that would have been
  supplied to the underlying compiler.

* ``--showme:command``: Outputs the underlying compiler
  command (which may be one or more tokens).

* ``--showme:incdirs``: Outputs a space-delimited (but otherwise
  undecorated) list of directories that the wrapper compiler would
  have provided to the underlying compiler to indicate
  where relevant header files are located.

* ``--showme:libdirs``: Outputs a space-delimited (but otherwise
  undecorated) list of directories that the wrapper compiler would
  have provided to the underlying linker to indicate where relevant
  libraries are located.

* ``--showme:libs`` Outputs a space-delimited (but otherwise
  undecorated) list of library names that the wrapper compiler would
  have used to link an application. For example: ``mpi open-pal
  util``.

* ``--showme:version``: Outputs the version number of Open MPI.

* ``--showme:help``: Output a brief usage help message.

See the man page for your underlying compiler for other options that
can be passed through mpicc.


DESCRIPTION
-----------

Conceptually, the role of these commands is quite simple:
transparently add relevant compiler and linker flags to the user's
command line that are necessary to compile / link Open MPI programs,
and then invoke the underlying compiler to actually perform the
command.

As such, these commands are frequently referred to as "wrapper"
compilers because they do not actually compile or link applications
themselves; they only add in command line flags and invoke the
back-end compiler.

Background
----------

Open MPI provides wrapper compilers for several languages:

* ``mpicc``: C

* ``mpic++``, ``mpicxx`` (and on systems with case-sensitive file
  systems, ``mpiCC``): C++

  .. note:: ``mpic++``, ``mpicxx``, and ``mpiCC`` all invoke the same
            underlying C++ compiler with the same options. All are
            provided as compatibility with other MPI implementations.

* ``mpifort`` (and its legacy/deprecated aliaes ``mpif77`` and
  ``mpif90``): Fortran

* ``mpijavac``: Java

The wrapper compilers for each of the languages are identical; they
can be use interchangeably.  The different names are provided solely
for backwards compatibility.


Fortran Notes
-------------

The Fortran wrapper compiler for MPI (``mpifort``, and its
legacy/deprecated names ``mpif77`` and ``mpif90``) can compile and
link MPI applications that use any/all of the MPI Fortran bindings:
``mpif.h``, the ``mpi`` module, and the ``mpi_f08`` module (assuming
Open MPI was installed with support for each of these Fortran
bindings). Specifically: it is no longer necessary to use different
wrapper compilers for applications that use ``mpif.h``
vs. applications that use the ``mpi`` module |mdash| just use ``mpifort``
for all Fortran MPI applications.

Note, however, that the Fortran compiler may require additional
command-line options to enforce a specific Fortran dialect. For
example, in some versions of the IBM XLF compiler, if ``xlf90`` is the
underlying Fortran compiler, ``-qfixed`` may be necessary to compile
fixed-format Fortran source files.

Finally, note that ``mpifort`` will be inoperative and will return an
error on use if Fortran support was not built into the MPI layer.

Overview
--------

``mpicc`` is a convenience wrappers for the underlying C compiler.
Translation of an Open MPI program requires the linkage of the Open
MPI-specific libraries which may not reside in one of the standard
search directories of ``ld(1)``. It also often requires the inclusion
of header files what may also not be found in a standard location.

``mpicc`` passes its arguments to the underlying C compiler along with
the ``-I``, ``-L`` and ``-l`` options required by Open MPI programs.

The same is true for all the other language wrapper compilers.

The Open MPI Team *strongly* encourages using the wrapper compilers
instead of attempting to link to the Open MPI libraries manually. This
allows the specific implementation of Open MPI to change without
forcing changes to linker directives in users' Makefiles. Indeed, the
specific set of flags and libraries used by the wrapper compilers
depends on how Open MPI was configured and built; the values can change
between different installations of the same version of Open MPI.

Indeed, since the wrappers are simply thin shells on top of an
underlying compiler, there are very, very few compelling reasons *not*
to use Open MPI's wrapper compilers. When it is not possible to use
the wrappers directly, the ``--showme:compile`` and ``--showme:link``
options should be used to determine what flags the wrappers would have
used. For example:

.. code:: sh

   shell$ cc -c file1.c `mpicc --showme:compile`

   shell$ cc -c file2.c `mpicc --showme:compile`

   shell$ cc file1.o file2.o `mpicc --showme:link` -o my_mpi_program


NOTES
-----

It is possible to make the wrapper compilers multi-lib aware. That is,
the libraries and includes specified may differ based on the compiler
flags specified (for example, with the GNU compilers on Linux, a
different library path may be used if ``-m32`` is seen versus ``-m64``
being seen). This is not the default behavior in a standard build, but
can be activated (for example, in a binary package providing both 32
and 64 bit support). `More information can be found here
<https://github.com/open-mpi/ompi/wiki/compilerwrapper3264>`_.


.. _man1-ompi-wrapper-compiler-files:

FILES
-----

The strings that the wrapper compilers insert into the command line
before invoking the underlying compiler are stored in a text file
created by Open MPI and installed to
``$pkgdata/NAME-wrapper-data.txt``, where:

* ``$pkgdata`` is typically ``$prefix/share/openmpi``
* ``$prefix`` is the top installation directory of Open MPI
* ``NAME`` is the name of the wrapper compiler (e.g.,
  ``$pkgdata/mpicc-wrapper-data.txt``)

It is rarely necessary to edit these files, but they can be examined to
gain insight into what flags the wrappers are placing on the command
line.


ENVIRONMENT VARIABLES
---------------------

By default, the wrappers use the compilers that were selected when
Open MPI was configured. These compilers were either found
automatically by Open MPI's "configure" script, or were selected by
the user in the ``CC``, ``CXX``, and/or ``FC`` environment variables
before ``configure`` was invoked. Additionally, other arguments specific
to the compiler may have been selected by configure.

These values can be selectively overridden by either editing the text
files containing this configuration information (see the :ref:`FILES
<man1-ompi-wrapper-compiler-files>` section), or by setting selected
environment variables of the form ``ompi_value``.

Valid value names are:

* ``CPPFLAGS``: Flags added when invoking the preprocessor (C or C++)

* ``LDFLAGS``: Flags added when invoking the linker (C, C++, or
  Fortran)

* ``LIBS``: Libraries added when invoking the linker (C, C++, or
  Fortran)

* ``CC``: C compiler

* ``CFLAGS``: C compiler flags

* ``CXX``: C++ compiler

* ``CXXFLAGS``: C++ compiler flags

* ``FC``: Fortran compiler

* ``FCFLAGS``: Fortran compiler flags
