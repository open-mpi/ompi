Feb 10, 2012
---------------
***************************************************************************
IMPORTANT NOTE

JAVA BINDINGS ARE PROVIDED ON A "PROVISIONAL" BASIS - I.E., THEY ARE
NOT PART OF THE CURRENT OR PROPOSED MPI STANDARDS. THUS, INCLUSION OF
JAVA SUPPORT IS NOT REQUIRED BY THE STANDARD. CONTINUED INCLUSION OF
THE JAVA BINDINGS IS CONTINGENT UPON ACTIVE USER INTEREST AND
CONTINUED DEVELOPER SUPPORT.

THIS SPECIFIC JAVA INTERFACE IS EXPERIMENTAL, INCOMPLETE (at least at
this time), AND SUBJECT TO CHANGE!

***************************************************************************

This version of Open MPI provides support for Java-based
MPI applications. At the time of this writing, not all MPI functions
are supported. However, work on extending the Java bindings to
provide full MPI coverage is underway.

The rest of this document provides step-by-step instructions on
building OMPI with Java bindings, and compiling and running
Java-based MPI applications

============================================================================

Building Java Bindings 

If this software was obtained as a developer-level
checkout as opposed to a tarball, you will need to start your build by
running ./autogen.pl. This will also require that you have a fairly
recent version of autotools on your system - see the HACKING file for
details.

Java support requires that Open MPI be built at least with shared libraries
(i.e., --enable-shared) - any additional options are fine and will not
conflict. Note that this is the default for Open MPI, so you don't
have to explicitly add the option. The Java bindings will build only
if --enable-mpi-java is specified, and a JDK is found in a typical
system default location.

If the JDK is not in a place where we automatically find it, you can
specify the location. For example, this is required on the Mac
platform as the JDK headers are located in a non-typical location. Two
options are available for this purpose:

--with-jdk-bindir=<foo> - the location of javac and javah
--with-jdk-headers=<bar> - the directory containing jni.h

For simplicity, typical configurations are provided in platform files
under contrib/platform/hadoop. These will meet the needs of most
users, or at least provide a starting point for your own custom
configuration.

In summary, therefore, you can configure the system using the
following Java-related options:

./configure --with-platform=contrib/platform/hadoop/<your-platform>
...

or

./configure --enable-mpi-java --with-jdk-bindir=<foo>
--with-jdk-headers=bar ...

or simply

./configure --enable-mpi-java ...

if jdk is in a "standard" place that we automatically find.

----------------------------------------------------------------------------

Running Java Applications

For convenience, the "mpijavac" wrapper compiler has been provided for
compiling Java-based MPI applications. It ensures that all required MPI
libraries and class paths are defined. You can see the actual command
line using the --showme option, if you are interested.

Once your application has been compiled, you can run it with the
standard "mpirun" command line:

mpirun <options> java <your-java-options> <my-app>

For convenience, mpirun has been updated to detect the "java" command
and ensure that the required MPI libraries and class paths are defined
to support execution. You therefore do NOT need to specify the Java
library path to the MPI installation, nor the MPI classpath. Any class
path definitions required for your application should be specified
either on the command line or via the CLASSPATH environmental
variable. Note that the local directory will be added to the class
path if nothing is specified.

As always, the "java" executable, all required libraries, and your application classes
must be available on all nodes.

----------------------------------------------------------------------------

If you have any problems, or find any bugs, please feel free to report
them to Open MPI user's mailing list (see
http://www.open-mpi.org/community/lists/ompi.php).
