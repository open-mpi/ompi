Static cluster configurations
=============================

.. error:: This entire section needs to be checked.

Clusters rarely change from day-to-day, and large clusters rarely
change at all.  If you know your cluster's configuration, there are
several steps you can take to both reduce Open MPI's memory footprint
and reduce the launch time of large-scale applications.  These steps
use a combination of build-time configuration options to eliminate
components |mdash| thus eliminating their libraries and avoiding
unnecessary component open/close operations |mdash| as well as
run-time MCA parameters to specify what modules to use by default for
most users.

One way to save memory is to avoid building components that will
actually never be selected by the system. Unless MCA parameters
specify which components to open, built components are always opened
and tested as to whether or not they should be selected for use. If
you know that a component can build on your system, but due to your
cluster's configuration will never actually be selected, then it is
best to simply configure OMPI to not build that component by using the
``--enable-mca-no-build`` configure option.

For example, if you know that your system will only utilize the
``ob1`` component of the PML framework, then you can ``no_build`` all
the others. This not only reduces memory in the libraries, but also
reduces memory footprint that is consumed by Open MPI opening all the
built components to see which of them can be selected to run.

In some cases, however, a user may optionally choose to use a
component other than the default.  For example, you may want to build
all of the PRRTE ``routed`` framework components, even though the vast
majority of users will simply use the default ``debruijn``
component.  This means you have to allow the system to build the other
components, even though they may rarely be used.

You can still save launch time and memory, though, by setting the
``routed=debruijn`` MCA parameter in the default MCA parameter file.
This causes OMPI to not open the other components during startup, but
allows users to override this on their command line or in their
environment so no functionality is lost |mdash| you just save some
memory and time.
