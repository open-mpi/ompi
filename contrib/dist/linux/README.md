# Open MPI Linux distribution helpers

Note that you probably want to download the latest release of the SRPM
for any given Open MPI version.  The SRPM release number is the
version after the dash in the SRPM filename.  For example,
`openmpi-1.6.3-2.src.rpm` is the 2nd release of the SRPM for Open MPI
v1.6.3.  Subsequent releases of SRPMs typically contain bug fixes for
the RPM packaging, but not Open MPI itself.

The `buildrpm.sh` script takes a single mandatory argument -- a
filename pointing to an Open MPI tarball (may be either `.gz` or
`.bz2`).  It will create one or more RPMs from this tarball:

1. Source RPM
1. "All in one" RPM, where all of Open MPI is put into a single RPM.
1. "Multiple" RPM, where Open MPI is split into several sub-package
   RPMs:
   * `openmpi-runtime`
   * `openmpi-devel`
   * `openmpi-docs`

The folowing arguments could be used to affect script behaviour.
Please, do NOT set the same settings with parameters and config vars.

* `-b`:
   If you specify this option, only the all-in-one binary RPM will
   be built. By default, only the source RPM (SRPM) is built. Other
   parameters that affect the all-in-one binary RPM will be ignored
   unless this option is specified.

* `-n name`:
   This option will change the name of the produced RPM to the "name".
   It is useful to use with "-o" and "-m" options if you want to have
   multiple Open MPI versions installed simultaneously in the same
   enviroment. Requires use of option `-b`.

* `-o`:
   With this option the install path of the binary RPM will be changed
   to `/opt/_NAME_/_VERSION_`. Requires use of option `-b`.

* `-m`:
   This option causes the RPM to also install modulefiles
   to the location specified in the specfile. Requires use of option `-b`.

* `-i`:
   Also build a debuginfo RPM. By default, the debuginfo RPM is not built.
   Requires use of option `-b`.

* `-f lf_location`:
   Include support for Libfabric. "lf_location" is Libfabric install
   path. Requires use of option `-b`.

* `-t tm_location`:
   Include support for Torque/PBS Pro. "tm_location" is path of the
   Torque/PBS Pro header files. Requires use of option `-b`.

* `-d`:
   Build with debugging support. By default,
   the RPM is built without debugging support.

* `-c parameter`:
   Add custom configure parameter.

   **NOTE:** As of Open MPI v5.0.x, there are default configure
   options for `--with-FOO=external` for all the 3rd party packages
   (libevent, hwloc, pmix, prrte).  This makes a pure Open MPI RPM,
   not an RPM that includes those 3rd party packages.  If you specify
   `-c`, if you want to preserve the default options, you will need to
   include those in the parameter value.

* `-r parameter`:
   Add custom RPM build parameter.

* `-s`:
   If specified, the script will try to unpack the openmpi.spec
   file from the tarball specified on the command line. By default,
   the script will look for the specfile in the current directory.

* `-R directory`:
   Specifies the top level RPM build direcotry.

* `-h`:
   Prints script usage information.


Target architecture is currently hard-coded in the beginning
of the `buildrpm.sh` script.

Alternatively, you can build directly from the `openmpi.spec` spec
file or SRPM directly.  Many options can be passed to the building
process via `rpmbuild`'s `--define` option (there are older versions
of `rpmbuild` that do not seem to handle `--define`'d values properly
in all cases, but we generally don't care about those old versions of
`rpmbuild`...).  The available options are described in the comments
in the beginning of the spec file in this directory.
