.. _building-pmix-cli-options-rpath-and-runpath-label:

Linker "rpath" and "runpath" functionality
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   
PMIx |opmix_ver| is composed of multiple
libraries that depend on each other.

When built from official distribution tarballs,
PMIx is built with the following versions of the
GNU Autotools:

* Autoconf |autoconf_min_version|
* Automake |automake_min_version|
* Libtool |libtool_min_version|

This set of GNU Autotools invokes the ``libtool`` executable to build
PMIx's libraries and executables with the `-rpath CLI option
<https://www.gnu.org/software/libtool/manual/html_node/Link-mode.html#Link-mode>`_.

The behavior of ``libtool -rpath ...`` is, unfortunately, highly
system-dependent.  In conjunction with compiler-, linker-, and other
system-level settings, the end result may be to effect "rpath"
behavior, "runpath" behavior, or possibly even neither behavior.

Although the specific behavior of ``libtool -rpath ..`` is outside the
scope of this documentation, you can run commands such as ``readelf -d
...`` to find out with which behavior your PMIx was built.

For example:

.. code::

   shell$ ./configure --prefix=/opt/pmix/ ...
   ...
   shell$ make -j 32 all && make install
   ...
   shell$ readelf -d /opt/pmix/lib/libmpi.so | egrep -i 'rpath|runpath'
    0x000000000000001d (RUNPATH)            Library runpath: [/opt/pmix/lib]

The above output indicates that ``libpmix.so`` was built with "runpath"
support, whereas output like this:

.. code::

   shell$ readelf -d /opt/pmix/lib/libpmix.so | egrep -i 'rpath|runpath'
    0x000000000000000f (RPATH)              Library rpath: [/opt/pmix/lib]

indicates that ``libpmix.so`` was built with "rpath" support.
          
.. note:: If you want to utilize additional compiler or linker flags
          (such as runpath flags) when building PMIx,
          you can :ref:`specify these flags on the configure command
          line <install-configure-compilers-and-flags-label>`.

          For example, if invoking ``libtool -rpath ...`` on your
          system actually effects "rpath" behavior, and you wish to
          effect "runpath" behavior, you could set ``LDFLAGS`` when
          invoking ``configure``, like this:

          .. code-block:: sh

             shell$ ./configure LDFLAGS=-Wl,--enable-new-dtags ...
