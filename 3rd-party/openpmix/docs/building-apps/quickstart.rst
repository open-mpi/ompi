.. _label-quickstart-building-apps:

Quick start: Building PMIx-based applications
=============================================

Although this section skips many details, it offers examples that will
probably work in many environments.

.. caution:: Note that this section is a "Quick start" |mdash| it does
   not attempt to be comprehensive or describe how to build PMIx
   in all supported environments.  The examples below may therefore
   not work exactly as shown in your environment.

   Please consult the other sections in this chapter for more details,
   if necessary.

PMIx provides a "wrapper" compiler (```pmixcc`` <man1-pmixcc>`)that can be used for
compiling PMIx-based applications. The intent is that users can simply invoke the
PMIx wrapper compiler instead of their usual language compiler (e.g., ``gcc``).
For example, instead of invoking your usual C compiler to build your
application, use ``pmixcc``:

.. code-block:: sh

   shell$ pmixcc hello_world_pmix.c -o hello_world_pmix
   shell$

All the wrapper compiler does is add a variety of compiler and linker
flags to the command line and then invoke a back-end compiler.  To be
specific: the wrapper compilers do not parse source code at all; they
are solely command-line manipulators, and have nothing to do with the
actual compilation or linking of programs.  The end result is anI
executable that is properly linked to all the relevant libraries.

.. caution:: It is *absolutely not sufficient* to simply add ``-lpmix``
             to your link line and assume that you will obtain a valid
             PMIx executable.

.. note:: You should use the wrapper compiler provided by your higher-level
          library instead of ``pmix`` if (a) your library provides a wrapper,
          and (b) your library also uses PMIx (in which case, its wrapper
          should not only include the necessary linkages, but also ensures
          that your application and library are both using the *same* PMIx
          library)
