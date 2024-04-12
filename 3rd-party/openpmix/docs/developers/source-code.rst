Source code
===========

Code style
----------

We intentionally do not have too many code conventions in the PMIx
code base.

All languages
^^^^^^^^^^^^^

* 4 space tabs.  No more, no less.
* **NEVER** use actual tab characters; always use spaces.  Both emacs
  and vim have secret mojo that can automatically use spaces when you
  hit the ``<TAB>`` key.  This makes the code look the same in every
  browser, regardless of individual tab display settings.

C / C++
^^^^^^^

* When comparing constants for equality or inequality, always put the
  constant on the left.  This is defensive programming: if you have a
  typo in the test and miss a ``!`` or ``=``, you'll get a compiler error.
  For example:

  .. code-block:: c

     /* Do this */
     if (NULL == foo) { ... }

     /* Because if you have a typo (i.e., = instead of ==), this will
        be a compile error rather than a subtle bug */
     if (NULL = foo) { ... }

* More defensive programming: *always* include blocks in curly braces
  ``{ }``, even if they are only one line long.  For example:

  .. code-block:: c

     /* Do this */
     if (whatever) {
         return OMPI_SUCCESS;
     }

     /* Not this */
     if (whatever)
        return OMPI_SUCCESS;

* PMIx requires a C99-compliant compiler.

  * C++-style comments are now allowed (and preferred).
  * C99-style mixing declarations are allow allowable (and preferred).

* **ALWAYS** include ``pmix_config.h`` as your first #include file.
  There are very, very few cases where
  this is not true (E.g., some bizarre Windows scenarios).  But in
  99.9999% of cases, this file should be included **first** so that it
  can affect system-level #include files if necessary.
* Filenames and symbols must follow the **prefix rule** (see [e-mail
  thread](http://www.open-mpi.org/community/lists/devel/2009/07/6389.php)):

  * Filenames must be prefixed with ``<framework>_<component>``.
  * Public symbols must be prefixed in components with
    ``pmix_<framework>_<component>``. When in doubt about
    whether a symbol is public, be safe and add the prefix.
  * Non-public symbols must be declared ``static`` or otherwise made to
    not appear in the global scope.

* **ALWAYS** #define macros, even for logical values.

  * The GNU Way is to ``#define`` a macro when it is "true" and to
    ``#undef`` it when it is "false".
  * In PMIx, we **always** ``#define`` a logical macro to be
    either 0 or 1 -- we never ``#undef`` it.
  * The reason for this is defensive programming: if you are only
    checking if a preprocessor macro is defined (via ``#ifdef FOO`` or
    ``#if defined(FOO)``), you will get no warning when compiling if
    you accidentally misspell the macro name.  However, if you use the
    logic test ``#if FOO`` with an undefined macro (e.g., because you
    misspelled it), you'll get a compiler warning or error.

    .. admonition:: Rationale
       :class: tip

       Misspelled macro names can be tremendously difficult to find
       when they are buried in thousands of lines of code; we will
       take all the help from the preprocessor/compiler that we can
       get!

  .. code-block:: c

     /* GNU Way - you will get no warning from the compiler if you
        misspell "FOO"; the test will simply be false */
     #ifdef FOO
     ...
     #else
     ...
     #endif

     /* PMIx Way - you will get a warning from the compiler if you
        misspell "FOO"; the result of the test is a different value
        than whether you spelled the macro name right or not */
     #if FOO
     ...
     #else
     ...
     #endif


Shell scripting
^^^^^^^^^^^^^^^

Please read some of the existing shell code in the source code tree
and try to use a similar style.

* Always enclose evaluated shell variables in quotes to ensure that
  multi-token values are handled properly.

  .. code-block:: sh

     # This is bad
     if test $foo = bar; then

     # This is good
     if test "$foo" = "bar"; then

  * The one exception to this is that when doing an assignment to a
    shell variable from another shell variable, it is not necessary to
    use quotes on the right hand side:

    .. code-block:: sh

       # This is harmless, but unnecessary
       foo="$bar"

       # This is actually sufficient, even for multi-token values of $bar
       foo=$bar

* Do not use the ``==`` operator for ``test`` |mdash| this is a GNU
  extension and can cause portability problems on BSD systems.
  Instead, use the single ``=`` operator.

  .. code-block:: sh

     # This is bad
     if test "$foo" == "bar"; then

     # This is good
     if test "$foo" = "bar"; then

m4
^^^

We do not have specific coding style guidelines for m4 (the language
used to create the ``configure`` script).  Please read some of the
existing m4 code in the source code tree and try to use a similar
style.

Tree layout
-----------

There are a few notable top-level directories in the source
tree:

* The main PMIx source is under the ``src`` directory
* ``config``: M4 scripts supporting the top-level ``configure`` script
* ``etc``: Some miscellaneous text files
* ``docs``: Source code for PMIx documentation
* ``examples``: Trivial example programs
* ``include``: The public PMIx headers

The ``src`` directory generates a top-level library named ``libpmix``.
It can be built as either a static or shared library. The directory
structure under it includes:

* ``class``: C++-like "classes" (using the OPAL class system)
  specific to this project
* ``include``: Top-level internal include files
* ``mca``: MCA frameworks and components specific to PMIx
* ``runtime``: Startup and shutdown of PMIx at runtime
* ``tools``: Executables specific to PMIx
* ``util``: Random utility code

The layout of the ``mca`` tree is strictly defined to be of the
form:

.. code-block:: text

    mca/FRAMEWORK/COMPONENT

To be explicit: it is forbidden to have a directory under the ``mca``
tree that does not meet this template (with the exception of ``base``
directories, explained below).  Hence, only framework and component
code can be in the ``mca`` tree.

That is, framework and component names must be valid directory names
(and C variables; more on that later).  For example, the CLIENT PTL
component is located in ``mca/ptl/client/``.

The name ``base`` is reserved; there cannot be a framework or component
named ``base``. Directories named ``base`` are reserved for the
implementation of the MCA and frameworks.  Here are a few examples (as
of the |opmix_series| source tree):

.. code-block:: sh

    # Main implementation of the MCA
    mca/base

    # Implementation of the ptl framework
    mca/ptl/base

    # Implementation of the client component of the ptl framework
    mca/ptl/client

Under these mandated directories, frameworks and/or components may have
arbitrary directory structures, however.

Symbol Visibility
-----------------

The ``PMIX_EXPORT`` macro provides a method to annotate symbols to indicate
their intended visibility when compiling dynamically shared object files
(e.g., ``libpmix.so``).

The macro expands to the appropriate compiler and platform flags for marking
whether a symbol should be explicitly made public in the PMIx
library namespace.
The ``PMIX_EXPORT`` attribute is used to declare that a symbol is to be
visible outside of the PMIx DSO's scope.

.. note:: This is entirely related to dynamic library compilation and does not
   apply to static compilation.
