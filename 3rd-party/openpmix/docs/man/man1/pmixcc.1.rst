.. _man1-pmixcc:

pmixcc
=========

.. include_body

pmixcc |mdash| wrapper compiler for PMIx-based applications or tools

SYNOPSIS
--------

``pmixcc [options] <file>``


DESCRIPTION
-----------

``pmixcc`` is a wrapper compiler that can be used to build PMIx-based
applications or tools.


OPTIONS
-------

``pmixcc`` accepts the following options:

* ``-h`` | ``--help``: Show help message

* ``--help={common|optimizers|params|target|warnings|[^]{joined|separate|undocumented}}[,...].``: Display specific types of command line options

* ``-v`` | ``--verbose``: Enable debug output.

* ``-V`` | ``--version``: Print version and exit.

* ``-dumpspecs``: Display all of the built in spec strings.

* ``-dumpversion``: Display the version of the compiler.

* ``-dumpmachine``: Display the compiler's target processor.

* ``-foffload=<targets>``: Specify offloading targets.

* ``-print-search-dirs``: Display the directories in the compiler's search path.

* ``-print-libgcc-file-name``: Display the name of the compiler's companion library.

* ``-print-file-name=<lib>``: Display the full path to library <lib>.

* ``-print-prog-name=<prog>``: Display the full path to compiler component <prog>.

* ``-print-multiarch``: Display the target's normalized GNU triplet, used as a component in the library path.

* ``-print-multi-directory``: Display the root directory for versions of libgcc.

* ``-print-multi-lib``: Display the mapping between command line options and multiple library search directories.

* ``-print-multi-os-directory``: Display the relative path to OS libraries.

* ``-print-sysroot``: Display the target libraries directory.

* ``-print-sysroot-headers-suffix``: Display the sysroot suffix used to find headers.

* ``-Wa,<options>``: Pass comma-separated <options> on to the assembler.

* ``-Wp,<options>``: Pass comma-separated <options> on to the preprocessor.
* ``-Wl,<options>``: Pass comma-separated <options> on to the linker.

* ``-Xassembler <arg>``: Pass <arg> on to the assembler.

* ``-Xpreprocessor <arg>``: Pass <arg> on to the preprocessor.

* ``-Xlinker <arg>``: Pass <arg> on to the linker.

* ``-save-temps``: Do not delete intermediate files.

* ``-save-temps=<arg>``: Do not delete intermediate files.

* ``-no-canonical-prefixes``: Do not canonicalize paths when building relative prefixes to other gcc components.

* ``-pipe``: Use pipes rather than intermediate files.

* ``-time``: Time the execution of each subprocess.

* ``-specs=<file>``: Override built-in specs with the contents of <file>.

* ``-std=<standard>``: Assume that the input sources are for <standard>.

* ``--sysroot=<directory>``: Use <directory> as the root directory for headers and libraries.

* ``-B <directory>``: Add <directory> to the compiler's search paths.

* ``-v``: Display the programs invoked by the compiler.

* ``-###``: Like -v but options quoted and commands not executed.

* ``-E``: Preprocess only; do not compile, assemble or link.

* ``-S``: Compile only; do not assemble or link.

* ``-c``: Compile and assemble, but do not link.

* ``-o <file>``: Place the output into <file>.

* ``-pie``: Create a dynamically linked position independent executable.

* ``-shared``: Create a shared library.

* ``-x <language>``: Specify the language of the following input files.
  Permissible languages include: c c++ assembler none
  'none' means revert to the default behavior of
  guessing the language based on the file's extension.


Options starting with ``-g``, ``-f``, ``-m``, ``-O``, ``-W``, or ``--param`` are automatically
passed on to the various sub-processes invoked by the compiler.  In order to pass
other options on to these processes the ``-W<letter>`` options must be used.


EXIT STATUS
-----------

Returns 0 if build is successful, a non-zero error code if otherwise.


EXAMPLES
--------

Examples of using this command.

.. seealso::
   :ref:`openpmix(5) <man5-openpmix>`
