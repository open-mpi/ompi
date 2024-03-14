.. _man1-oshcc:
.. _man1-oshcxx:
.. _man1-oshc++:
.. _man1-oshfort:
.. _man1-shmemcc:
.. _man1-shmemcxx:
.. _man1-shmemc++:
.. _man1-shmemfort:


OpenSHMEM Wrapper Compilers
===========================

.. include_body

oshcc, oshcxx, oshc++, oshfort, shmemcc, shmemcxx, shmemc++, shmemfort |mdash| OpenSHMEM wrapper compilers


SYNTAX
------

``oshcc [--showme | --showme:compile | --showme:link] ...``

``oshcxx [--showme | --showme:compile | --showme:link] ...``

``oshc++ [--showme | --showme:compile | --showme:link] ...``

``oshfort [--showme | --showme:compile | --showme:link] ...``

``shmemcc [--showme | --showme:compile | --showme:link] ...``

``shmemcxx [--showme | --showme:compile | --showme:link] ...``

``shmemc++ [--showme | --showme:compile | --showme:link] ...``

``shmemfort [--showme | --showme:compile | --showme:link] ...``


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
            flags. For example, both ``"oshcc --showme`` and
            ``oshcc --showme my_source.c`` will show all the
            wrapper-supplied flags. But ``oshcc
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

See the man page for your underlying compiler for other
options that can be passed through oshcc.


DESCRIPTION
-----------

Conceptually, the role of these commands is quite simple: transparently
add relevant compiler and linker flags to the user's command line that
are necessary to compile / link OpenSHMEM programs, and then invoke the
underlying compiler to actually perform the command.

As such, these commands are frequently referred to as "wrapper"
compilers because they do not actually compile or link applications
themselves; they only add in command line flags and invoke the back-end
compiler.

Background
----------

Open MPI provides wrapper compilers for several languages:

* ``oshcc``, ``shmemcc``: C

* ``oshc++``, ``oshcxx``, ``shmemc++``, shmemcxx`:: C++

* ``oshfort``, ``shmemfort``: Fortran

The wrapper compilers for each of the languages are identical; they
can be use interchangeably.  The different names are provided solely
for backwards compatibility.


Fortran Notes
-------------

The Fortran wrapper compiler for OpenSHMEM (``oshfort`` and
``shmemfort``) can compile and link OpenSHMEM applications that use
any/all of the OpenSHMEM Fortran bindings.

Note, however, that the Fortran compiler may require additional
command-line options to enforce a specific Fortran dialect. For
example, in some versions of the IBM XLF compiler, if ``xlf90`` is the
underlying Fortran compiler, ``-qfixed`` may be necessary to compile
fixed-format Fortran source files.

Finally, note that ``oshfort`` will be inoperative and will return an
error on use if Fortran support was not built into the OpenSHMEM
layer.

Overview
--------

``oshcc`` and ``shmemcc`` are convenience wrappers for the underlying
C compiler.  Translation of an OpenSHMEM program requires the linkage
of the OpenSHMEM-specific libraries which may not reside in one of the
standard search directories of ``ld(1)``. It also often requires the
inclusion of header files what may also not be found in a standard
location.

``oshcc`` and ``shmemcc`` pass their arguments to the underlying C
compiler along with the ``-I``, ``-L`` and ``-l`` options required by
OpenSHMEM programs.

The same is true for all the other language wrapper compilers.

The OpenSHMEM Team *strongly* encourages using the wrapper compilers
instead of attempting to link to the OpenSHMEM libraries manually. This
allows the specific implementation of OpenSHMEM to change without
forcing changes to linker directives in users' Makefiles. Indeed, the
specific set of flags and libraries used by the wrapper compilers
depends on how OpenSHMEM was configured and built; the values can change
between different installations of the same version of OpenSHMEM.

Indeed, since the wrappers are simply thin shells on top of an
underlying compiler, there are very, very few compelling reasons *not*
to use ``oshcc``. When it is not possible to use the wrappers
directly, the ``--showme:compile`` and ``--showme:link`` options should be
used to determine what flags the wrappers would have used. For example:

.. code:: sh

   shell$ cc -c file1.c `shmemcc --showme:compile`

   shell$ cc -c file2.c `shmemcc --showme:compile`

   shell$ cc file1.o file2.o `shmemcc --showme:link` -o my_oshmem_program


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


.. _man1-oshmem-wrapper-compiler-files:

FILES
-----

The strings that the wrapper compilers insert into the command line
before invoking the underlying compiler are stored in a text file
created by OpenSHMEM and installed to
``$pkgdata/NAME-wrapper-data.txt``, where:

* ``$pkgdata`` is typically ``$prefix/share/openmpi``
* ``$prefix`` is the top installation directory of OpenSHMEM
* ``NAME`` is the name of the wrapper compiler (e.g.,
  ``$pkgdata/shmemcc-wrapper-data.txt``)

It is rarely necessary to edit these files, but they can be examined to
gain insight into what flags the wrappers are placing on the command
line.


ENVIRONMENT VARIABLES
---------------------

By default, the wrappers use the compilers that were selected when
OpenSHMEM was configured. These compilers were either found
automatically by Open MPI's "configure" script, or were selected by
the user in the ``CC``, ``CXX``, and/or ``FC`` environment variables
before ``configure`` was invoked. Additionally, other arguments specific
to the compiler may have been selected by configure.

These values can be selectively overridden by either editing the text
files containing this configuration information (see the :ref:`FILES
<man1-oshmem-wrapper-compiler-files>` section), or by setting selected
environment variables of the form ``oshmem_value``.

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
