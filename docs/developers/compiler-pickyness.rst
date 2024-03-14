Compiler Pickyness by Default
=============================

If you are building Open MPI from a Git clone (i.e., there is a
``.git`` directory in your build tree), the default build includes
extra compiler pickyness, which will result in more compiler warnings
than in non-developer builds.  Getting these extra compiler warnings
is helpful to Open MPI developers in making the code base as clean as
possible.

Developers can disable this picky-by-default behavior by using the
``--disable-picky`` configure option.  Also note that extra-picky
compiles do *not* happen automatically when you do a VPATH build
(e.g., if ``.git`` is in your source tree, but not in your build
tree).

Prior versions of Open MPI would automatically activate a lot of
(performance-reducing) debugging code by default if ``.git`` was found
in your build tree.  This is no longer true.  You can manually enable
these (performance-reducing) debugging features in the Open MPI code
base with these configure options:

* ``--enable-debug``
* ``--enable-mem-debug``
* ``--enable-mem-profile``

.. note:: These options are really only relevant to those who are
   developing Open MPI itself.  They are not generally helpful for
   debugging general MPI applications.
