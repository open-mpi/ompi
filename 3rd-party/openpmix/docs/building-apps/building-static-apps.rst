.. _label-building-fully-static-apps:

Building fully-static PMIx applications
=======================================

Is fully-static really what you need?
-------------------------------------

No one ends up in this documentation section by accident.

If you are reading this text, it is likely because you are looking to
solve a problem, and fully-static applications sound like the
right answer.  There are two common problems that people think that
fully-static applications will solve:

#. Applications fail to launch because dependent shared libraries
   are not found at run-time.

#. Filesystem performance when launching at scale is terrible.

If either of these are your problems, the PMIx community *strongly
encourages you to use other mechanisms to fix the problem*:
fully-static applications are *possible*, but are sub-optimal for
other reasons.

The following sections discuss the above problems.

Finding dependent shared libraries at run-time
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you are running into problems finding shared libraries at run time
|mdash| particularly on nodes that are remote from where you have invoked
your launcher |mdash| your best bet is to set ``LD_LIBRARY_PATH`` (or
equivalent mechanism) properly on *all* nodes (including remote
nodes).

This is an involved topic, but even so, it is generally simpler to
solve this problem than creating and maintaining static builds.

Improving filesystem performance at scale
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Prior to v4.2.0, PMIx compiled a large number of plugins as
individual dynamic shared objects (DSOs) |mdash| i.e., individual
files in the filesystem.  Many of these DSOs would be opened by each
application process at run time.

This could cause filesystem congestion, particularly when the application is
installed on a network filesystem and a large job is launched: many
nodes will simultaneously communicate with the file server(s), and
potentially need to transfer a large number of small(ish) files.

Starting with v4.2.0, by default, PMIx's plugins are no longer
built as DSOs.  As such, PMIx typically only opens a small number
of shared libraries at launch time.  Even if PMIx is installed on
a network filesystem, these libraries are likely to be cached on nodes
over time, and therefore generate a fairly small amount network
filesystem traffic when PMIx-based jobs are launched.

In short: PMIx |opmix_ver|'s impact on network filesystems is
greatly diminished compared to prior versions.  Compiling fully-static
applications to eliminate the open-every-DSO-file-at-launch-time
behavior is no longer necessary.

Other reasons fully-static applications are bad
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here are a few other reasons that fully-static applications are
sub-optimal:

#. When applications link all of their dependencies statically, the
   operating system cannot share code between multiple copies of the
   process.

   For example, if you launch N copies of your fully-statically-linked
   application on a node, it will consume (N *
   size_of_the_application) bytes of RAM.  Alternately, launching N
   copies of a dynamically-linked MPI application |mdash| where each
   of the copies have the same dependent libraries |mdash| will only
   load each shared dependent library into RAM once.

   In other words: *using dynamic linking saves memory.*

#. If you disable PMIx's ``dlopen`` functionality (which is
   necessary to create *fully*-static MPI applications), you may
   lose access to functionality that is dependent on third-party
   libraries that cannot operate (or are not provided) as static
   libraries. Note that your application also cannot use a
   memory manager (such as most MPI libraries do by default).

Are you convinced yet?  *Please try to avoid building fully-static
applications if at all possible.*


Building fully-static PMIx-based applications
---------------------------------------------

.. caution:: If, after reading all of the above, you are still of the
             mind that you want to build fully-static
             applications, be aware that fully static linking is not
             for the meek, and it is not recommended.  But it is
             possible, with some caveats.

#. You must have static libraries available for *everything* to which
   your program links.  This includes PMIx; you must have used the
   ``--enable-static`` option to PMIx's ``configure`` or otherwise
   have available the static versions of the PMIx library.

   .. note:: Some Linux distributions may not have static versions of
             popular Linux libraries by default (e.g., libnuma), or
             require additional RPMs to be installed to get the
             equivalent static libraries.

#. Your application must have been built without a memory manager.
   For example, this means that Open MPI must have been configured with the
   ``--without-memory-manager`` flag.

   .. important:: Not including memory manager support can lead to
                  lower performance when using
                  OS-bypass networks.

This is how to configure PMIx to build fully-static libraries on
Linux:

.. code-block:: sh

   shell$ ./configure --disable-dlopen \
       --enable-static --disable-shared ...

The ``--disable-shared`` flag is optional; it will prevent PMIx
from *also* building shared libraries.

Alternatively, you could build PMIx with as many static libraries
as possible, but still preserve ``dlopen`` functionality by omitting
the ``--disable-dlopen`` flag:

.. code-block:: sh

   shell$ ./configure --enable-static --disable-shared ...

This gives you a *mostly* static build of PMIx, but has the
advantage of preserving at least some dynamic libraries.

Including whole archives
^^^^^^^^^^^^^^^^^^^^^^^^

Some systems may have additional constraints about their support
libraries that require additional steps to produce working
fully-static PMIx-based applications.  For example, any library that has its
own run-time plugin system (i.e., that opens dynamically shared
objects ("DSOs") at run time) will have additional complications in
producing fully-static builds.

In such cases, you generally want to run ``pmixcc ... --showme`` to see
the compiler / linker commands that PMIx's wrapper commands will
use, and then augment those commands with linker arguments for the
static versions of the DSO plugins that you will need at run time.

For example, if you have ``libfoo.a`` that dynamically loads
``plugin.so`` at run time, you'll need to have a ``plugin.a`` and
|mdash| assuming the GNU linker |mdash| add arguments similar to the
following:

* ``-static``: Tell the linker to generate a static executable.
* ``-Wl,--whole-archive -lfoo /path/to/plugin.a -Wl,--no-whole-archive``:
  Tell the linker to include the entire ``foo`` library and the entire
  ``plugin.a`` archive in the executable.

You can either add these arguments on the command line manually, or
you can :ref:`modify the default behavior of the wrapper compilers
<label-customizing-wrapper-compiler>` to hide this complexity from end
users (but be aware that if you modify the wrapper compilers' default
behavior, *all* users will be creating static applications!).
