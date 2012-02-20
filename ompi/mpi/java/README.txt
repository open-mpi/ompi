This set of Java bindings was originally derived from mpiJava v1.2.7:

    http://sourceforge.net/projects/mpijava/  

The 1.2.7 tarball was uploaded to SourceForce on April 8, 2011
(although the README in the 1.2.7 tarball claims that it is version
1.2.5, and is dated January 2003).

There are home pages on the internet for "mpiJava" that might well be
ancestors of this project:

    http://www.hpjava.org/mpiJava.html
    http://aspen.ucs.indiana.edu/pss/HPJava/mpiJava.html

The source code and configure/build system have been heavily modified
to be in Open MPI.  

The README file from the original 1.2.7 tarball is included below.
The License.txt file the 1.2.7 tarball is included in this directory.

=========================================================================

                  mpiJava - A Java Interface to MPI
                  ---------------------------------

                     Version 1.2.5, January 2003

              Bryan Carpenter, Sung Hoon Ko, Sang Boem Lim
              Pervasive Technology Labs, Indiana University
              email {shko,slim,dbc}@grids.ucs.indiana.edu

                             Xinying Li
                         Syracuse University

                             Mark Baker
                   CSM, University of Portsmouth
                   email mark.baker@computer.org


This package provides an object-oriented Java interface to the Message
Passing Interface (MPI) standard, for use on parallel or distributed
computing platforms.  The release includes the Java Native Interface
(JNI) C stubs that binds the Java interface to an underlying native MPI
C interface (which must be obtained and installed independently).  The
release also includes a comprehensive test suite for the Java
interface, created by translating the IBM MPI test suite to Java.
It includes some simple examples and demos.

The Java API is defined in the document "mpiJava 1.2: API specification"
in the `doc/' directory.


Platforms
---------

We have tested this release on the platforms listed below.

For the marked configurations, please note remarks in later sections
of this README.  Some platforms need special configuration options for
the native MPI.  All test cases and examples in this release have been run
on all platforms.  Except for few occasions where programs completed but
terminated awkwardly, no failures were observed, provided the recommended
configuration was followed.

        Operating System        Java                    Native MPI

        Redhat Linux 7.3        Sun SDK 1.4.1           MPICH 1.2.5
        Redhat Linux 7.3        Sun SDK 1.4.1           LAM 6.5.8
        Redhat Linux 7.3        IBM JDK 1.4.0           MPICH 1.2.4   (*)
        Redhat Linux 7.3        IBM JDK 1.4.0       	LAM 6.5.8     (*)
        SunOS 5.8               Sun SDK 1.4.1           SunHPC-MPI 4
        SunOS 5.8               Sun SDK 1.4.1           MPICH 1.2.5   (*)
        SunOS 5.8               Sun SDK 1.4.1           LAM 6.5.8     (*)
        AIX 3.4                 IBM JDK 1.3.0           IBM MPI (SP2/3)

The software was also tested on the following platform, but occasional
intermittent failures were observed:

        AIX 3.4                 IBM JDK 1.3.0           MPICH 1.2.5   (*)

See the file TEST_REPORTS for more information.

(*): Note the remarks in the section below on MPI configuration options.

Of course it is possible to build mpiJava on other platforms, but expect
to do some trouble-shooting.

At various times we have successfully tested versions of this software
using combinations of systems including:

  1. Operating systems:

       Sun Machines running SunOS 5.4 (Solaris2.5.1)
       Redhat Linux 7.3
       AIX
       WinTel NT 4 (SP3)
       SGI Challange Machines running IRIX 6.2

  2. Java Development environments:

       Sun SDK 1.4(Linux, Solaris)
       IBM Developer Kit for Linux, J2RE 1.4.0
       AIX JDK 1.3
       Java JDK 1.1.x(SGI)
   
  3. Native MPI installations:

       MPICH 1.2.5
       SunHPC-MPI(4.0) 
       IBM POE
       WMPI 1.1

Users have reported ports to other platforms (for Alpha processors, see
the message at the end of this file).

Updates and further information about mpiJava can be found on the home
page, `www.hpjava.org/mpiJava.html'.

If you find bugs, have comments, or want further information about
mpiJava or the team of developers, email `dbcarpen@indiana.edu' or
`sblim@indiana.edu'.


Installation
------------

The following instructions apply to UNIX.  

[Some earlier releases of mpiJava have been tested on Windows NT.  This
release has *not*.  For old instructions on installation under Windows NT,
see the file `NT_INSTALL.TXT'.  Because recent releases of mpiJava haven't
been tested on that platform, those instructions will certainly need
updating---we leave this option available for "experts" only.]


1. Install your preferred Java programming environment.

For Solaris and Linux, we strongly recommend to use JDK 1.4 or later.
This release provides a signal-chaining feature that can be used to
avoid some unpleasant non-deterministic bugs encountered with previous
releases of mpiJava.

After Java JDK is installed successfully, you should add the Java JDK
`bin' directory to your path setting, so that the `mpiJava/configure'
script can find the `java', `javac', and `javah' commands.


2. Install your preferred MPI software.

Add the MPI `bin' directory to your path setting.  Test the MPI
installation before attempting to install mpiJava!

(See the "Recommended MPI configuration options" sections below,
for some recommended options when installing MPI.)


3. Now, you are ready to install the mpiJava interface.

   step 1.  Unpack the software, eg

              gunzip -c mpiJava-x.x.x.tar.gz | tar -xvf -

            A subdirectory `mpiJava/' is created.

   step 2.  Go to the `mpiJava/' directory.  Configure the software for
            your platform:

              ./configure

            You may specify various standard options to the configure
            process.
	    
            Try

                ./configure --help

            for various option.

            The default MPI is MPICH.  Use

                ./configure --with-MPI=lam

            for LAM.  Use

                ./configure --with-MPI=sunhpc

            for SunHPC.  Use

                ./configure --with-MPI=sp2

            for AIX + POE.

   step 3.  Build (compile) the software:

                make


After successful compilation, the makefile will put the generated class 
files in directory `lib/classes/mpi/', and also place a native dynamic
library in directory `lib/'.  Now:

  Add the directory `<mpiJava-pathname>/src/scripts' to your path environment
  variable.

  Add the directory `<mpiJava-pathname>/lib/classes' to your CLASSPATH
  environment variable.

  Add the directory `<mpiJava-pathname>/lib' to your LD_LIBRARY_PATH
  (Linux, Solaris, etc) or LIBPATH (AIX) environment variable.

(Some of these variables may be unnecesary if you are using the
`prunjava' script.)

   step 4.  Test the installation:

                make check


NOTE: Several of the the scripts in this release assume your target
machines share user directories (presumably through NFS or equivalent),
and have compatible system commands *and library files* installed on
all nodes (e.g. in `/usr/lib').  Although it is possible to adapt the
basic mpiJava software to more heterogeneous situations, you will need
to do more work!


Using the software
------------------

If everything goes well, you can compile and run the test programs by
issuing the command

  make check

in the mpiJava installation directory.

An example of how to compile and run a program:

  javac Life.java
  prunjava 4 Life

The `prunjava' script is a wrapper for the various MPI run commands.
The first argument is the number of processors on which the program will be
executed. A list of available host computers may be given in an
MPICH-style `machines' file in the local directory.

The `prunjava' script is provided mainly for purposes of testing.  It is
not very general and in real situations you will often have to modify
this script, or start the program directly using the native MPI run
commands to achieve the effect you need.

With MPICH on some platforms you may be able to run mpiJava programs by

  mpirun <mpirun options> java <java command arguments>

With this approach, you may be responsible for ensuring the remote
environment is set up correctly, e.g. by setting appropriate class
paths and library paths in your `.cshrc', `.bashrc', etc, on the remote
machines (the `prunjava' script adopts a different approach it
dynamically creates a script that sets up the required environment and
invokes the `java' command.  This script is run across nodes using
`mpirun'.)

On SP2 you might run mpiJava by

  poe java <java command arguments> <poe options>

Some MPI environments (SunHPC 4.0) may require that the native MPI library
be preloaded into the executable command---it may not be possible to
load the native `libmpi' with the Java `System.loadLibrary()' method.
Preloading can be achieved in Solaris or Linux by setting the LD_PRELOAD
environment variable.  So for example with SunHPC you may start mpiJava by:

  LD_PRELOAD=/opt/SUNWhpc/lib/libmpi.so
  export LD_PRELOAD
  mprun <mprun options> java <java command arguments>

(It is best to restrict of the LD_PRELOAD variable scope
by defining it only within a script, like our `prunjava'.  Otherwise the
library may get loaded into *every* executable you run!
For reliable operation you should also add the `libjsig' library, where
available, to the LD_PRELOAD variable.  See the notes below.  Check the
source of the `mpiJava/src/scripts/prunjava' script for examples.)


API
---

The API definition is in

  mpiJava/doc/api/mpi/mpiJava-spec.ps

Javadoc documentation for the API is preinstalled at

  mpiJava/doc/api/mpi/package-summary.html

For questions and comments, email us.  


Recommended MPI configuration options
-------------------------------------

In many case mpiJava will work using default MPI options.  But after
much experimentation the options recommended below have been found to
eliminate certain failure modes.  See the technical notes below for
more discussion.

Note all `configure' options specified in this section are for MPICH
or LAM `configure' scripts, *not* mpiJava!


1) Redhat Linux 7.3 + Sun SDK 1.4.1 + MPICH 1.2.5

Default


2) Redhat Linux 7.3 + Sun SDK 1.4.1 + LAM 6.5.6

Default is recommended.

If, however, problems are encountered, you may try reconfiguring LAM to
use a different signal, e.g.:

     ./configure ... --with-signal=SIGIO


3) Redhat Linux 7.3 + IBM 1.4 Java for Linux + MPICH 1.2.4

MPICH must be configured to use a signal other than the default SIGUSR1,
e.g.:

     ./configure ... -listener_sig=SIGIO


4) Redhat Linux 7.3 + IBM 1.4 Java for Linux + LAM 6.5.8

LAM must be configured to use a signal other than the default SIGUSR2,
e.g.:

     ./configure ... --with-signal=SIGIO


5) SunOS 5.8 + Sun SDK 1.4.1 + SunHPC-MPI 4

Default.


6) SunOS 5.8 + Sun SDK 1.4.1 + MPICH 1.2.4

Use:

    ./configure ... -cflags=-D_REENTRANT

(Note: on Solaris mpiJava has been tested with MPICH built using cc.)


7) SunOS 5.8 + Sun SDK 1.4.1 + LAM 6.5.6

Use:

    ./configure ... --with-cflags=-D_REENTRANT


8) AIX 3.4 + IBM JDK 1.3.0 Java + IBM MPI (SP2/3)

Default


9) AIX 3.4 + IBM JDK 1.3.0 Java + MPICH 1.2.5

Use:

   ./configure ... -cflags=-D_THREAD_SAFE

Note however that certain test cases have been observed to intermittently
hang on this platform for unknown reasons.  It's use is not recommended.
(Note: on AIX mpiJava has been tested with MPICH built using cc.)


                           Technical Notes
                           ===============

The following technical notes and case studies are largely for the benefit
of people trying to port mpiJava to other platforms, but in some cases
they also bear on the required configuration of the native MPI...


Problems with Signal Handlers (mpiJava 1.2.5)
---------------------------------------------

A problem in porting mpiJava to different platforms is conflicts in
uses of OS signal handlers by the Java Virtual Machine (and Java
libraries) and by the native MPI implementation.

Typical JVMs make use of OS signals and signal-handlers internally.
Typical MPI implementations override the default signal handlers.
If suitable measures are not taken, the MPI may blindly override the
signal-handlers installed by the JVM, leading to failures.

If you are using Sun's Java, we recommended to upgrade to JDK 1.4,
and set the environment variable `LD_PRELOAD' described in

  http://java.sun.com/j2se/1.4/docs/guide/vm/signal-chaining.html

For example:

  export LD_PRELOAD=$JAVA_HOME/jre/lib/$JARCH/$VM/libjsig.so

This resolves various intermittent bugs reported with previous versions
of mpiJava (on many important platforms).

In some cases this option is not sufficient or not available.  Sometimes
it is nevertheless possible to work around problems by saving the signal
handlers installed by JVM, and restoring them after the MPI has overriden
them.  The current release of mpiJava introduces a second native library
for saving and restoring relevant signal handlers.  In other cases it may
be possible and/or necessary to reconfigure MPI to use a "safe" signal.

[In the following notes we have tried to give plausible causes for
observed problems.  But appearances can be deceptive and we don't always
have access to sources of the software concerned; even where we do,
it can be very labour intensive to trace intermittent failure modes
in detail.  Nevertheless we hope the workarounds we found may suggest
ones that work in other situations.]


KNOWN SIGNAL-HANDLING ISSUES for specific platforms, with workarounds:

The workarounds are configured in automatically for mpiJava 1.2.5 where
appropriate, but in some cases you may have to change your native MPI
configuration to avoid conflicting signals.

    1) Redhat Linux 7.3 + Sun SDK 1.4.1 + MPICH 1.2.5

    Hotspot sometimes deliberately throws and catches SIGSEGV and
    similar signals.  `MPI_Init' overrides the JVM signal handlers
    leading to intermittent failures (especially in complex recursive
    code, like object serialization).  With earlier versions of JDK
    many mpiJava programs ran successfully despite this conflict.

    JDK 1.4 signal-chaining using `libjsig' resolves all remaining issues
    we are aware of.  This is configured automatically into the mpiJava
    1.2.5 `prunjava' script, if mpiJava is built with JDK 1.4.


    2) Redhat Linux 7.3 + Sun SDK 1.4.1 + LAM 6.5.6

    We expect the same issues with SIGSEGV, etc as in MPICH case, which
    should be resolved by using `libjsig'.

    Additionally, there is a special problem with SIGUSR2, which causes
    frequent, intermittent hanging of mpiJava programs.  Just loading
    `libjsig' doesn't resolve this problem (the signal handlers don't
    seem to chain properly?)  We found empirically that restoring the
    original JVM signal handler for SIGUSR2 after `MPI_Init' eliminated
    problems in all our test cases.  This approach is automatically 
    configured into mpiJava 1.2.5.

    An alternative solution is to configure LAM to use a signal
    that Hotspot doesn't use, e.g.:

        ./configure ... --with-signal=SIGIO

    (Note well this is the `configure' script for LAM, *not* mpiJava!
    We randomly suggested SIGIO as the alternate signal.)


    3) Redhat Linux 7.3 + IBM 1.4 Java for Linux + MPICH 1.2.4

    The IBM classic JVM uses SIGUSR1, and (we found) may block this signal
    during JNI calls.  By default MPICH (on the default P4 device) uses
    SIGUSR1 as its listener signal.  This conflict causes most mpiJava
    programs to hang.  The only known solution is to to configure MPICH
    to use a different signal, e.g:

        ./configure ... -listener_sig=SIGIO

    (Note well this is the `configure' script for MPICH, *not* mpiJava!
    We randomly suggested SIGIO rather than the more obvious SIGUSR2.
    SIGUSR2 mostly worked, but apparently produced conflicts in GUI-based
    example codes.)

    This resolves all problems we are currently aware of.


    4) Redhat Linux 7.3 + IBM 1.4 Java for Linux + LAM 6.5.8

    We had some success.  But the `tests/signals/' test case and
    `examples/Nozzle', `examples/potts' examples hang on some of our
    installations.  Configuring LAM to use e.g. SIGIO -- see 2), above
    -- appeared to help, but we aren't certain this is a complete
    solution -- we had conflicting experiences.

    For now this configuration should be considered experimental.


    5) SunOS 5.8 + Sun SDK 1.4.1 + SunHPC-MPI 4

    Comments similar to the Linux MPICH case, 1).  No known problems
    provide the `libjsig' signal interception library is loaded.


    6) SunOS 5.8 + Sun SDK 1.4.1 + MPICH 1.2.5

    Comments similar to the Linux case, 1) above, except that on Solaris
    the 1.4 JVM detects the occurrence of signal chaining it doesn't like,
    and insists the java option "-Xusealtsigs" be set.  This is configured
    automatically into the mpiJava 1.2.5 `prunjava' script.

    SEE ALSO the notes on thread safety issues, below.

    (Note: on Solaris mpiJava has been tested assuming MPICH is built
    with cc.)


    7) SunOS 5.8 + Sun SDK 1.4.1 + LAM 6.5.6

    Comments similar to the Linux MPICH case, 1).  No known problems.

    SEE ALSO the notes on thread safety issues, below.


    8) AIX 3.4 + IBM JDK 1.3.0 Java + IBM MPI (SP2/3)

    The JVM sometimes deliberately throws and catches SIGTRAP signals
    (in a pattern similar to SIGSEGV, etc with Hotspot?), and the SP2
    MPI apparently overrides the JVM handler.  We know of no `libjsig'
    analogue for this platform, but we found empirically that restoring
    the original JVM signal handler for SIGTRAP after the
    `System.loadLibrary(mpijava)' call eliminated problems in all our
    test cases.  This solution is automatically configured into mpiJava
    1.2.5.


    9) AIX 3.4 + IBM JDK 1.3.0 Java + MPICH 1.2.5

    Certain test cases have been observed to intermittently hang on this
    platform for unknown reasons.  It's use is not recommended.

    SEE ALSO the notes on thread safety issues, below.

    (Note: on AIX the mpiJava configure script assumes MPICH is built
    with cc, not GNU C.)


Issues of Thread Safety (mpiJava 1.2.5)
---------------------------------------

Most MPI implementations are not "thread-safe", and of course Java
uses threads in an essential way---even a single-threaded user program
will have system daemon threads running in the background.

In principle this could be a serious issue for mpiJava.  To make
progress we have mainly disregarded the problem, and worked on the
optimistic assumption that provided *MPI* CALLS ARE NEVER MADE
CONCURRENTLY (and, by the way, it is *your* responsibility as the mpiJava
programmer to ensure this!) interference between Java threads should
not cause problems.

A priori this is not guaranteed.  The native MPI implementation might
be making OS system calls to send messages over sockets.  Daemon
threads or other user threads could also (through the standard Java
API) be concurrently making system calls (e.g. an AWT program could be
communicating with an X server).  If the MPI implementation happens not
to invoke its system calls in a thread-safe way, there could still be
interference effects with the system calls invoked internally by the
other "pure Java" threads.  (One example is that the MPICH
implementation relies on the `errno' variable; in principle this
could be modified by other threads.)

We have not encountered problems that were *provably* attributable to
this kind of effect.  But we *have* encountered problems with graphics
codes (e.g. `examples/Nozzle', `example/potts') running on the Solaris
+ MPICH, Solaris + LAM and AIX + MPICH platforms that look suspiciously
like this.  With the default build of MPICH and LAM, these programs
usually fail on these platforms.

Experimentally we found that on Solaris these problems could be eliminated by
reconfiguring MPICH to compile with the flag `-D_REENTRANT':

  ./configure ... -cflags=-D_REENTRANT

and similarly configuring LAM as follows:

  ./configure ... --with-cflags=-D_REENTRANT

(Note well these are the `configure' scripts for MPICH and LAM,
*not* mpiJava!)

On AIX the corresponding recipe that worked was:

  ./configure ... -cflags=-D_THREAD_SAFE

(Note well this is for the `configure' scripts for MPICH, not mpiJava!
Unfortunately we failed to install LAM on AIX.  As noted above AIX
+ MPICH has other problems, which are unresolved.)

We were unable to trace the detailed cause of the observed failures, so
it is not 100% certain whether this is really a thread safety issue.
But in general setting `-D_REENTRANT' on Solaris or `-D_THREAD_SAFE'
on AIX would be expected to improve the thread safety characteristics
of C code.

Another change in this release related to thread safety is in the
implementation of the `finalize()' methods of the `Datatype', `Group',
`Op' and `Status' classes.  In earlier releases of mpiJava these were
native methods that directly called the corresponding `MPI_Free'
functions.  Although this wasn't observed to cause problems, in principle
it is not thread safe because the `finalize()' methods may be called in
a separate garbage collector thread.  In the current release the calls
to the native methods are deferred, and invoked in the user thread when
the next MPI operation is explicitly called.


JVMs and "pinning" (mpiJava 1.2.3)
----------------------------------

The garbage collectors associated with early JVMs, such as the
"classic" JVM, supported pinning of Java arrays---fixing the arrays
to a specific physical location while a JNI call was in progress.
Several more modern JVMs (e.g. Hotspot and others) do not support
pinning.  Instead JNI calls access elements of Java arrays by first obtaining
a C copy of the Java array.  The elements are typically copied back
from the C array to the Java array when the JNI call returns.

mpiJava 1.2.3 supports two approaches to message buffers, reflecting
these two JNI mechanisms---pinning or copying.  If you are using a
JVM which is known to support pinning, you may wish to uncomment the
definition of the macro `GC_DOES_PINNING' in the file `src/C/mpiJava.h'.

If this macro is left undefined---presumably meaning the garbage
collector does *not* support pinning---mpiJava will copy buffers
from and to Java arrays explicitly using `MPI_Pack' and `MPI_Unpack'.
This works well with MPICH.  

Unfortunately this strategy doesn't always work with IBM MPI,
due to an apparent difference in the semantics of `MPI_Unpack'.  
Luckily it turns out that many installations of Java on AIX still use
a variant of the classic JVM, which *does* support pinning.  So on AIX
it is probably safest to define the `GC_DOES_PINNING' macro.

[Note added: the `configure' script now attempts to determine whether
the JVM supports pinning and will define the `GC_DOES_PINNING' macro in
make files, if it thinks it does.]


Revision History
----------------

Significant changes from version 1.2.4:

1) Fixes various problems associated with signal handlers
   (see discussion above).

2) README file greatly extended to better document supported platforms and
   portability issues.

3) Fixes a bug related to the behavior of `MPI_Unpack' on certain
   MPI platforms.

4) Fixed some programming errors in the `examples/potts' and
   `examples/metropolis' codes.

5) No longer use custom `jvmlauncher' for SunHPC.  Instead use
   LD_PRELOAD to preload -lmpi library into standard `java' command.

6) Moves freeing of native MPI objects out of the garbage collector
   thread, into MPI user thread (no particular problems were observed
   with the old strategy, but in principle it isn't thread-unsafe).


Significant changes from version 1.2.3:

1) Supports SunHPC version 4.0.  Executable `src/bin/jvmlauncher' added.


Significant changes from version 1.2.2:

1) Supports AIX + POE platform.


Significant changes from version 1.2.1:

1) Major reorganization in handling communication buffers, the better to
   support current JVMs, whose garbage collectors often don't implement
   pinning.

2) Fix related bug in `Sendrecv', afflicting the `Life.java' example.

3) Fix bug reported by Jatinder Singh when `MPI.ANY_SOURCE' is used with
   and `MPI.OBJECT' datatype.

Significant changes from version 1.2:

1) Mainly bug fixes.


Significant changes from version 1.1:

1) Support for the `MPI.OBJECT' basic type (note that this release
   uses default JDK serialization, which can be quite inefficient).

2) Support for Linux platforms.

3) Inclusion of new demo programs.

4) Inclusion of `javadoc' documentation.

5) Other minor changes to the API---see the spec in the `doc' directory.

6) Bug fixes.


Known bugs and omissions
------------------------

1) The subclasses of `MPIException' documented in the mpiJava spec are still
   not implemented (and in reality mpiJava methods never throw
   exceptions---they generally abort the program in case of error).

2) In general, sanity-checking method arguments is not nearly as thorough
   as it should be.


mpiJava Directory Structure
---------------------------
  
     mpiJava/
         bin/
	      This directory contains binaries or installed scripts.
	      For NT releases, sub-directories contain Win32 Dynamic
	      Link Libraries (.dll).

              WMPI/
		   For NT releases, contains wmpi.dll created by
		   compiling the JNI C stubs. The directory where the
		   DLL resides needs to be added to the PATH
		   environment variable so that it can be found at
		   run-time by Java.

                   mpiJava.dll 

         doc/

         examples/
              metropolis/
                  A Monte Carlo program

              Nozzle/
                  A CFD program, with GUI

              PingPong/
                  A simple benchmark, with C and Java versions

              potts/
                  Another Monte Carlo program, with a GUI

              simple/
                  A "Game of Life" program; a "Hello World" program.

         lib/
	      For UNIX releases this directory contains shared libraries.
              Class files are contained in a subdirectory.

              classes/
		   The mpiJava class files live here.  This directory
		   should be added to your CLASSPATH enviroment
		   variable.

                   mpiJava.zip

         src/
              C/
		   The JNI C stubs for mpiJava.  This directory
		   contains the JNI C wrappers and the header files for
		   mpiJava.  These files are compiled into a shared
		   (.so in UNIX) or dynamic-load-library (.dll in
		   Win32) that is loaded at runtime by the JVM
		   (loadlibary(mpiJava)) when the Java MPI interface is
		   used.

              Java/
		   The Java interface to MPI.  This directory includes
		   a sub-directory (mpi) holding the Java interface to
		   MPI.  These files need to be compiled using a Java
		   compiler, such as javac.  The resulting class files
		   are copied into the mpiJava/lib/classes directory.

                   mpi/

              scripts/
		   Various scripts for configuraing and testing mpiJava
		   under UNIX.

              wmpi_jni/
                   See notes in `NT_INSTALL.TXT'

                   release/

              bin/
                   The `jvmlauncher' program

         tests/
              ccl/
              comm/
              dtyp/
              env/
              group/
              pt2pt/
              topo/



                           References
                           ==========

MPI Home Page:
    http://www.mcs.anl.gov/mpi/index.html

MPICH home page:
    http://www.mcs.anl.gov/mpi/mpich

LAM home page:
    http://www.lam-mpi.org/

WMPI (an MPI for Windows NT):
    http://dsg.dei.uc.pt/w32mpi/

Sun J2SE 1.4 download:
    http://java.sun.com/j2se/1.4/download.html

IBM Java Developer Kit for Linux:
    http://www.ibm.com/java/jdk/download


Contributions
-------------

From Hiromitsu Takagi:

I'd like to inform you that we have successfully built and run it on
Digital UNIX V4.0D (OSF JDK1.1.6) / MPICH but a few modifications are
required.

o add "-I$(JDK)/include/java -I$(JDK)/include/java/alpha" into 
  INCLUDE of mpiJava-1.1/src/C/Makefile
  (jni.h is placed on $(JDK)/include/java/ and jni_md.h is placed on
  $(JDK)/include/alpha/.)

o set LDFLAG of mpiJava-1.1/src/C/Makefile "-shared"

[...]
--
Hiromitsu Takagi
Computer Science Division, Electrotechnical Laboratory

Sep 1, 98

     ---=+ O +=---

Thanks to Rutger Hofman who pointed out a bug in `Request.Waitany',
`Request.Testany' and gave corrections.

Feb 28, 01

     ---=+ O +=---

     The test case in `tests/signals/' is adapted from a bug
report submitted by Sivakumar Venkata Pabolu.

Jan 10, 03


