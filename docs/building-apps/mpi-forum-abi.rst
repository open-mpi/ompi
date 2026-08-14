.. _label-building-with-mpi-abi:

Building MPI applications using the MPI Forum ABI
=================================================

.. note:: Open MPI supports two ABIs:

          * **The Open MPI ABI:** this is the ABI that Open MPI has
            supported for multiple releases over many years.  It is
            provided by the ``libmpi`` library.

          * **The MPI Forum ABI:** this is the official ABI as defined
            by the MPI standard.  It was not supported in Open MPI
            until v6.0.0.  It is provided by the ``libmpi_abi``
            library.

          These two ABIs are different and not interchangeable.

          **Open MPI builds support for both ABIs by default.**  A
          single Open MPI installation therefore provides both
          ``libmpi`` and ``libmpi_abi``, and can compile and run
          applications that use either ABI.  See
          :ref:`label-mpi-abi-enable-disable`, below, for how to
          disable the MPI Forum ABI support.

This section describes the **MPI Forum ABI** support in Open MPI.

For information about the Open MPI ABI, see
:ref:`ABI compatibility to previous versions of Open MPI
<label-binary-compatibility>`.

What is the MPI Forum ABI?
--------------------------

Starting with MPI-5.0, the MPI standard defines an Application Binary
Interface (ABI) for MPI applications.  In principle, an application can
be compiled against one MPI implementation that supports the MPI Forum
ABI, and later run with another MPI implementation that supports the
same MPI Forum ABI.

There are a few important limitations:

* The application must be dynamically linked.
* The application must be launched with a launcher that is suitable for
  the MPI library used at run time.
* This release of Open MPI supports building C MPI applications against
  the MPI Forum ABI.  It does not provide a Fortran ABI compiler
  wrapper.
* Open MPI's non-standard extensions are not available to applications
  built against the MPI Forum ABI.  See :ref:`Open MPI extensions are
  not available in the MPI Forum ABI
  <label-mpi-abi-no-extensions>`, below.

The MPI Forum ABI is different than Open MPI's own ABI compatibility
within an Open MPI release series.  See :ref:`Open MPI ABI
compatibility <label-binary-compatibility>` for information about
running applications that were compiled with an earlier Open MPI
release.

The version of the MPI Forum ABI defined by MPI-5.0 is 1.0.  Open MPI
installs the MPI Forum ABI library as ``libmpi_abi`` when MPI Forum ABI
support is enabled.

.. _label-mpi-abi-enable-disable:

Enabling and disabling MPI Forum ABI support
---------------------------------------------

Open MPI builds support for **both** the Open MPI ABI and the MPI Forum
ABI by default.  No ``configure`` option is needed to get the MPI Forum
ABI: a default Open MPI installation provides ``libmpi`` (the Open MPI
ABI) and ``libmpi_abi`` (the MPI Forum ABI), along with the compiler
wrappers and ``pkg-config`` files for each.

The MPI Forum ABI support is controlled by the ``--enable-standard-abi``
/ ``--disable-standard-abi`` ``configure`` options, which are described
with the rest of the :doc:`MPI functionality configure options
</installing-open-mpi/configure-cli-options/mpi>`.

.. note:: If Open MPI is configured with ``--disable-standard-abi``,
          ``libmpi_abi``, the ``mpicc_abi`` wrapper, and the
          ``ompi-abi*`` pkg-config files are not built or installed.
          Such an installation supports only the Open MPI ABI: it
          cannot compile MPI Forum ABI applications, and cannot serve
          as the run-time MPI library for them.

          The Open MPI ABI is always built; there is no option to
          disable it.

How to build an application using the MPI Forum ABI
----------------------------------------------------

Use the ``mpicc_abi`` compiler wrapper to compile and link C MPI
applications against the MPI Forum ABI:

.. code-block:: sh

   shell$ mpicc_abi hello.c -o hello

Then launch the application with the ``mpirun`` from the MPI
implementation that will provide ``libmpi_abi`` at run time:

.. code-block:: sh

   shell$ mpirun -np 2 ./hello

When using Open MPI's ``mpicc_abi``, the wrapper adds the MPI Forum ABI
header directory and links against ``libmpi_abi``.  You can inspect the
wrapper-provided flags with:

.. code-block:: sh

   shell$ mpicc_abi --showme:compile
   shell$ mpicc_abi --showme:link

The MPI Forum ABI ``mpi.h`` is installed under
``$prefix/include/standard_abi``.  The normal Open MPI ``mpi.h`` remains
installed under ``$prefix/include`` and is used by the normal ``mpicc``
wrapper.

Using ``pkg-config`` with the MPI Forum ABI
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As an alternative to using the ``mpicc_abi`` wrapper, you can use
``pkg-config`` to build MPI Forum ABI applications:

.. code-block:: sh

   shell$ export PKG_CONFIG_PATH=/opt/openmpi/lib/pkgconfig
   shell$ gcc hello.c -o hello `pkg-config ompi-abi-c --cflags --libs`

Open MPI provides the following ABI pkg-config files:

* ``ompi-abi``: Synonym for ``ompi-abi-c``
* ``ompi-abi-c``: C applications using the MPI Forum ABI
* ``ompi-abi-cxx``: C++ applications using the MPI Forum ABI

.. note:: These pkg-config files are only installed when Open MPI is
          configured with ``--enable-standard-abi`` (the default).

Like the ``mpicc_abi`` wrapper, these pkg-config files link only
against ``libmpi_abi`` and use the MPI Forum ABI header from
``$prefix/include/standard_abi``.

Checking which ABIs an installation provides
---------------------------------------------

Use :ref:`ompi_info(1) <man1-ompi_info>` to see which ABIs a given Open
MPI installation was built with:

.. code-block:: sh

   shell$ ompi_info | grep ABI
            Open MPI ABI: yes
           MPI Forum ABI: yes

The ``Open MPI ABI`` line is always ``yes``: Open MPI always builds its
own ABI.  The ``MPI Forum ABI`` line reflects whether the installation
was configured with the MPI Forum ABI support enabled (see
:ref:`label-mpi-abi-enable-disable`, above); it is ``yes`` by default,
and ``no`` in an installation configured with
``--disable-standard-abi``.

The same values are available in ``ompi_info``'s machine-readable
output, which is more convenient for scripts:

.. code-block:: sh

   shell$ ompi_info --parsable | grep bindings:abi
   bindings:abi:ompi:yes
   bindings:abi:mpi_forum:yes

Checking ABI support at run time
--------------------------------

Applications can call :ref:`MPI_Abi_get_version` to query the ABI
version provided by the MPI library at run time.  When running against
Open MPI's MPI Forum ABI library, this routine returns ABI version 1.0.

The normal Open MPI ``libmpi`` library also provides the
``MPI_Abi_get_version`` routine, but it returns ``-1`` for both the
major and minor version numbers because the normal ``libmpi`` library
uses Open MPI's own ABI, not the MPI Forum ABI.

An application is bound to one MPI ABI when it is compiled
----------------------------------------------------------

When you compile and link an MPI application, you choose one of the two
ABIs, and the resulting object files, libraries, and executables are
bound to that ABI:

* Compiling and linking with ``mpicc_abi`` (or the ``ompi-abi-c``
  pkg-config file) binds the application to the **MPI Forum ABI** and
  links it against ``libmpi_abi``.

* Compiling and linking with the normal ``mpicc`` / ``mpifort``
  wrappers (or the ``ompi-c`` / ``ompi-fort`` pkg-config files) binds
  the application to the **Open MPI ABI** and links it against
  ``libmpi``.

The two ABIs are *source*-compatible: the same MPI source code can be
compiled either way.  They are not *binary*-compatible with each other.

.. important:: The choice of ABI is made at compile time and cannot be
               changed afterward.  To move an already-built application
               from one MPI ABI to the other, you must recompile and
               relink it from source.  There is no way to convert a
               compiled object file, library, or executable from one MPI
               ABI to the other.

An application built against the MPI Forum ABI is *not* tied to
Open MPI, however.  That is the whole point of the MPI Forum ABI: the
same executable can run with any MPI implementation that provides the
MPI Forum ABI, as described next.

.. _label-mpi-abi-cross-implementation:

Running an application with a different MPI implementation
----------------------------------------------------------

An application compiled against the MPI Forum ABI can be run
with a *different* MPI implementation than the one it was built with, as
long as that other implementation also provides the MPI Forum ABI.  For
example, you can compile an application with Open MPI's ``mpicc_abi``
and then run it with MPICH, or vice versa.

Recall the requirements from the start of this section:

* The application must be dynamically linked.
* The application must be built against the MPI Forum ABI (for Open MPI,
  with ``mpicc_abi``).
* The application must be launched with |mdash| and must find at run
  time the MPI Forum ABI library of |mdash| the MPI implementation you
  want to run it with.

Building the other implementation with MPI Forum ABI support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An MPI implementation only provides the MPI Forum ABI library if it was
built with MPI Forum ABI support enabled.  Open MPI builds ``libmpi_abi``
by default (see ``--enable-standard-abi``).  Other implementations may
gate their MPI Forum ABI library behind a build-time option and may not
build it by default.

For example, at the time of this writing, MPICH provides its MPI Forum
ABI wrapper (``mpicc_abi``) and library (``libmpi_abi``) only when it is
configured with ``--enable-mpi-abi``:

.. code-block:: sh

   # Building MPICH with MPI Forum ABI support
   shell$ ./configure --prefix=/opt/mpich --enable-mpi-abi
   shell$ make
   shell$ make install

Consult the other implementation's own documentation for its current
option names and for the state of its MPI Forum ABI support; that
support, and its degree of conformance, varies between implementations
and between releases.  If an application does not behave correctly when
run against another implementation's MPI Forum ABI library, a good first
step is to try a more recent release of that implementation.

.. note:: The MPI Forum ABI is still relatively new.  Early
          releases of any implementation |mdash| including Open MPI
          |mdash| may have MPI Forum ABI bugs that are fixed in later
          releases.  When testing cross-implementation interoperability,
          prefer current releases of both implementations.

Finding the right library at run time
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The MPI Forum ABI library that both implementations provide has the same
name, so an application built against the MPI Forum ABI does not record
*which* implementation's library it needs |mdash| only that it needs the
MPI Forum ABI library.  At run time, the dynamic linker resolves that
dependency to whichever copy of the library it finds first.  You select
an implementation by making sure the dynamic linker finds *that*
implementation's copy.

The mechanism is the platform's normal shared-library search path:

* On Linux (and most other Unix-like systems), set ``LD_LIBRARY_PATH``
  to the ``lib`` directory of the implementation you want to run with.

* On macOS, set ``DYLD_LIBRARY_PATH`` instead.

Open MPI's wrappers record Open MPI's own library directory in the
executable (as an ``RUNPATH`` on ELF platforms), so that an application
built against Open MPI finds Open MPI's libraries by default with no
environment variables set.  ``LD_LIBRARY_PATH`` / ``DYLD_LIBRARY_PATH``
are searched *ahead* of that recorded path, so setting them is enough to
redirect the application to a different implementation's library; you do
not need to rebuild or relink the application.

.. note:: You must also launch the application with the target
          implementation's launcher (its ``mpiexec`` / ``mpirun``).  The
          launcher and the run-time MPI library are two halves of the
          same implementation and cannot be mixed: you cannot use Open
          MPI's ``mpiexec`` to launch a job that will use MPICH's
          ``libmpi_abi`` at run time, or vice versa.

Run-time parameters are not portable across implementations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The MPI Forum ABI standardizes the *application* binary
interface |mdash| the symbols, types, and constants that a compiled MPI
application uses.  It does **not** standardize how an MPI implementation
is configured and tuned at run time.

Each MPI implementation has its own, separate mechanisms for controlling
run-time behavior, and these are **not** interchangeable:

* **Launcher command-line options.** The options accepted by ``mpiexec``
  / ``mpirun`` are implementation-specific.  Open MPI, for example, uses
  MCA parameters (``--mca ...``); other implementations use entirely
  different option syntaxes.

* **Environment variables.** Open MPI reads ``OMPI_MCA_*`` (and related)
  environment variables; other implementations read their own,
  differently named variables.

* **Configuration files** and other tuning mechanisms likewise differ.

In other words, an application *binary* built against the MPI Forum ABI
is portable across implementations, but the *command line, environment,
and tuning knobs* you use to run it are not.  A launch script written
for one implementation generally has to be rewritten for another, even
though the application itself does not need to be recompiled.

Example: two installation prefixes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Suppose Open MPI is installed under ``/opt/openmpi`` and MPICH (built
with MPI Forum ABI support) is installed under ``/opt/mpich``.  Build the
application once, against Open MPI's MPI Forum ABI:

.. code-block:: sh

   shell$ /opt/openmpi/bin/mpicc_abi hello.c -o hello

Run it with Open MPI |mdash| its own libraries are found automatically:

.. code-block:: sh

   shell$ /opt/openmpi/bin/mpiexec -n 2 ./hello
   ... runtime library: Open MPI ...

Now run the *same executable* with MPICH.  Use MPICH's launcher, and put
MPICH's library directory on the loader search path so the MPI Forum ABI
library resolves to MPICH's copy:

.. code-block:: sh

   # Linux
   shell$ export LD_LIBRARY_PATH=/opt/mpich/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
   shell$ /opt/mpich/bin/mpiexec -n 2 ./hello
   ... runtime library: MPICH ...

On macOS, use ``DYLD_LIBRARY_PATH`` instead of ``LD_LIBRARY_PATH``:

.. code-block:: sh

   # macOS
   shell$ export DYLD_LIBRARY_PATH=/opt/mpich/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}
   shell$ /opt/mpich/bin/mpiexec -n 2 ./hello
   ... runtime library: MPICH ...

The application was compiled and linked exactly once; only the launcher
and the loader search path differ between the two runs.

.. note:: As elsewhere in this documentation, the shell commands in
          these examples use ``sh``-family (POSIX / Bourne) shell syntax
          |mdash| for example, ``sh``, ``bash``, ``zsh``, ``ksh``, or
          ``dash``.  If you use a ``csh``-family shell, adjust the
          environment-variable syntax accordingly.

Example: replacing one implementation with another in the same prefix
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can also build an application against one implementation, then
replace that implementation with another at the *same* installation
prefix, and run the same executable without changing your environment.
This works because the application looks for the MPI Forum ABI library by
name in the same location; installing a different implementation's
MPI Forum ABI library at that same location satisfies the dependency.

First install Open MPI under ``/opt/mpi``, build the application, and run
it:

.. code-block:: sh

   shell$ ./configure --prefix=/opt/mpi     # ...plus your Open MPI options
   shell$ make install
   shell$ /opt/mpi/bin/mpicc_abi hello.c -o hello
   shell$ /opt/mpi/bin/mpiexec -n 2 ./hello
   ... runtime library: Open MPI ...

Now remove Open MPI and install MPICH (built with MPI Forum ABI support)
under the same ``/opt/mpi`` prefix:

.. code-block:: sh

   shell$ rm -rf /opt/mpi
   shell$ ./configure --prefix=/opt/mpi --enable-mpi-abi   # MPICH
   shell$ make install

The same ``hello`` executable |mdash| not recompiled |mdash| now runs
with MPICH, using the ``mpiexec`` that is now installed at
``/opt/mpi/bin``.  No ``LD_LIBRARY_PATH`` / ``DYLD_LIBRARY_PATH`` is
needed, because the MPI Forum ABI library is found at the same path as
before:

.. code-block:: sh

   shell$ /opt/mpi/bin/mpiexec -n 2 ./hello
   ... runtime library: MPICH ...

.. note:: Install the replacement implementation into the prefix with
          its own build system (``configure --prefix=/opt/mpi`` followed
          by ``make install``), rather than by copying files.  On macOS
          in particular, the MPI Forum ABI library records its own
          absolute install path, which is set correctly only when the
          library is installed into that prefix by its build system.

Fortran support
---------------

As of summer 2026, Open MPI intentionally does not provide support for
the MPI Forum Fortran MPI Forum ABI for the following reasons:

* No other MPI implementation supports the MPI Forum Fortran ABI yet.

* The MPI Forum continues to discuss corrections and updates to the
  MPI Forum ABI.

Given this, it may actually be counter-productive for Open MPI to
implement the MPI Forum Fortran ABI at this time.  If the Forum's
Fortran ABI specification needs breaking changes, making those changes
becomes much more difficult once there are real-world implementations
of that Fortran ABI in use by actual MPI applications.

Open MPI has therefore taken a wait-and-see approach: we will let the
dust settle and let the MPI Forum work out the remaining issues and
converge on a final Fortran ABI specification before committing to a
Fortran MPI Forum ABI implementation.

Specifically: this release of Open MPI does not provide a
``mpifort_abi`` compiler wrapper, and does not provide
``libmpifort_abi``.

Open MPI does provide the MPI Forum ABI query/helper routines documented
in the MPI API man pages, including their Fortran bindings where the
normal Open MPI Fortran bindings are available.  Those routines do not
imply that this release provides a complete Fortran MPI Forum ABI build
path.

.. _label-mpi-abi-no-extensions:

Open MPI extensions are not available in the MPI Forum ABI
----------------------------------------------------------

Open MPI provides a number of :ref:`Open MPI extensions
<ompi-features-extensions-label>`: non-standard functions and constants,
named with the ``OMPI_*`` and ``MPIX_*`` prefixes, such as
``OMPI_Affinity_str()``, ``MPIX_Query_cuda_support()``, and the ULFM
``MPIX_Comm_*`` fault-tolerance routines.

.. warning:: The Open MPI extensions are **not** part of the MPI Forum
             ABI, and are therefore not present in ``libmpi_abi``.

             An application compiled with ``mpicc_abi`` |mdash| or with
             the ``ompi-abi-c`` pkg-config file |mdash| cannot call the
             Open MPI extensions.  Their ``OMPI_*`` and ``MPIX_*``
             symbols will not be found at link time.

This is intentional.  The purpose of the MPI Forum ABI is that an
application built against it can run with *any* MPI implementation that
provides that ABI.  Implementation-specific extensions are meaningless
in that context, since no other MPI implementation provides them.  Other
MPI implementations that support the MPI Forum ABI likewise omit their
own extensions from their MPI Forum ABI library.

The Open MPI extensions remain fully available in the normal Open MPI
``libmpi`` library.  An application that needs them must therefore use
the Open MPI ABI: compile and link it with the normal ``mpicc`` /
``mpifort`` wrapper compilers (or the ``ompi-c`` / ``ompi-fort``
pkg-config files).

Mixing Open MPI's ABI and the MPI Forum ABI
----------------------------------------------

The Open MPI ABI and the MPI Forum ABI are separate binary interfaces.

.. danger:: Do not mix object files or libraries that use both MPI
            ABIs in a single executable.

For example, the following cases are **not** supported:

* Compiling some C source files that call MPI with ``mpicc_abi`` and
  compiling other C source files that call MPI with ``mpicc``, then
  linking them into one executable.
* Compiling C source files that call MPI with ``mpicc_abi`` and
  compiling Fortran source files that call MPI with ``mpifort``, then
  linking them into one executable.
* Linking a library whose MPI-using object files were built with
  ``mpicc_abi`` into an application whose MPI-using object files were
  built with ``mpicc`` or ``mpifort``.

These combinations are unsafe because the process-wide ``MPI_*``
symbols must resolve to one MPI library ABI, while the object files were
compiled with different binary representations for MPI handles,
constants, callback arguments, and status objects.  The result is
undefined behavior; the executable may fail to link, crash, report MPI
errors, or appear to work until an MPI object crosses the ABI boundary.

Use *one* MPI ABI consistently for every object file that calls MPI:

* For a C MPI application using the MPI-5.0 MPI Forum ABI, compile and
  link all MPI-using C objects with ``mpicc_abi``.
* For other MPI applications, including mixed C and Fortran
  applications, compile C objects with ``mpicc`` and Fortran objects
  with ``mpifort``.

All processes in an MPI job must use the same MPI ABI
-----------------------------------------------------

The requirement to pick a single MPI ABI is not limited to an
individual executable: **every process in an MPI job must use the same
MPI ABI.**

You cannot run an MPI job in which some processes use the Open MPI ABI
(executables built with ``mpicc`` / ``mpifort`` and linked against
``libmpi``) while other processes use the MPI Forum ABI
(executables built with ``mpicc_abi`` and linked against
``libmpi_abi``).  This is true even though both kinds of process are
ultimately run by the same Open MPI installation.

.. danger:: Do not launch an MPI job that mixes processes built against
            the Open MPI ABI with processes built against the MPI Forum
            MPI Forum ABI.  Every process in the job must use the same
            MPI ABI.

This restriction applies to every way that processes can end up in the
same MPI job, including:

* **Multiple Program, Multiple Data (MPMD) launches**, where a single
  ``mpirun`` command starts more than one executable.  For example, the
  following is **not** supported if ``./app`` and ``./app_abi`` were
  built against different MPI ABIs:

  .. code-block:: sh

     shell$ mpirun -np 2 ./app : -np 2 ./app_abi

* **Dynamically connected jobs**, where processes that were launched
  separately are joined into a single MPI job at run time via
  :ref:`MPI_Comm_spawn` / :ref:`MPI_Comm_spawn_multiple`,
  :ref:`MPI_Comm_connect` and :ref:`MPI_Comm_accept`, or
  :ref:`MPI_Comm_join`.  All of the participating executables must be
  built against the same MPI ABI.

Build every executable that will participate in a given MPI job against
the same MPI ABI, and launch the job accordingly.
