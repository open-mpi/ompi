***************************************************************************
IMPORTANT NOTE

JAVA BINDINGS ARE PROVIDED ON A "PROVISIONAL" BASIS - I.E., THEY ARE
NOT PART OF THE CURRENT OR PROPOSED MPI STANDARDS. THUS, INCLUSION OF
JAVA SUPPORT IS NOT REQUIRED BY THE STANDARD. CONTINUED INCLUSION OF
THE JAVA BINDINGS IS CONTINGENT UPON ACTIVE USER INTEREST AND
CONTINUED DEVELOPER SUPPORT.

***************************************************************************

This version of Open MPI provides support for Java-based
MPI applications. 

The rest of this document provides step-by-step instructions on
building OMPI with Java bindings, and compiling and running
Java-based MPI applications. Also, part of the functionality is
explained with examples. Further details about the design,
implementation and usage of Java bindings in Open MPI can be found
in [1]. The bindings follow a JNI approach, that is, we do not
provide a pure Java implementation of MPI primitives, but a thin
layer on top of the C implementation. This is the same approach
as in mpiJava [2]; in fact, mpiJava was taken as a starting point
for Open MPI Java bindings, but they were later totally rewritten.

 [1] O. Vega-Gisbert, J. E. Roman, and J. M. Squyres. "Design and
     implementation of Java bindings in Open MPI". In preparation
     (2013).

 [2] M. Baker et al. "mpiJava: An object-oriented Java interface to
     MPI". In Parallel and Distributed Processing, LNCS vol. 1586,
     pp. 748-762, Springer (1999).

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

$ ./configure --with-platform=contrib/platform/hadoop/<your-platform>
...

or

$ ./configure --enable-mpi-java --with-jdk-bindir=<foo>
              --with-jdk-headers=<bar> ...

or simply

$ ./configure --enable-mpi-java ...

if JDK is in a "standard" place that we automatically find.

----------------------------------------------------------------------------

Running Java Applications

For convenience, the "mpijavac" wrapper compiler has been provided for
compiling Java-based MPI applications. It ensures that all required MPI
libraries and class paths are defined. You can see the actual command
line using the --showme option, if you are interested.

Once your application has been compiled, you can run it with the
standard "mpirun" command line:

$ mpirun <options> java <your-java-options> <my-app>

For convenience, mpirun has been updated to detect the "java" command
and ensure that the required MPI libraries and class paths are defined
to support execution. You therefore do NOT need to specify the Java
library path to the MPI installation, nor the MPI classpath. Any class
path definitions required for your application should be specified
either on the command line or via the CLASSPATH environmental
variable. Note that the local directory will be added to the class
path if nothing is specified.

As always, the "java" executable, all required libraries, and your
application classes must be available on all nodes.

----------------------------------------------------------------------------

Basic usage of Java bindings

There is an MPI package that contains all classes of the MPI Java
bindings: Comm, Datatype, Request, etc. These classes have a direct
correspondence with classes defined by the MPI standard. MPI primitives
are just methods included in these classes. The convention used for
naming Java methods and classes is the usual camel-case convention,
e.g., the equivalent of MPI_File_set_info(fh,info) is fh.setInfo(info),
where fh is an object of the class File.

Apart from classes, the MPI package contains predefined public attributes
under a convenience class MPI. Examples are the predefined communicator
MPI.COMM_WORLD or predefined datatypes such as MPI.DOUBLE. Also, MPI
initialization and finalization are methods of the MPI class and must
be invoked by all MPI Java applications. The following example illustrates
these concepts:

import mpi.*;
 
class ComputePi {

    public static void main(String args[]) throws MPIException {

        MPI.Init(args);
    
        int rank = MPI.COMM_WORLD.getRank(),
            size = MPI.COMM_WORLD.getSize(),
            nint = 100; // Intervals.
        double h = 1.0/(double)nint, sum = 0.0;
    
        for(int i=rank+1; i<=nint; i+=size) {
            double x = h * ((double)i - 0.5);
            sum += (4.0 / (1.0 + x * x));
        }
    
        double sBuf[] = { h * sum },
               rBuf[] = new double[1];
    
        MPI.COMM_WORLD.reduce(sBuf, rBuf, 1, MPI.DOUBLE, MPI.SUM, 0);
    
        if(rank == 0) System.out.println("PI: " + rBuf[0]);
        MPI.Finalize();
    }
}

----------------------------------------------------------------------------

Exception handling

Java bindings in Open MPI support exception handling. By default, errors
are fatal, but this behavior can be changed. The Java API will throw
exceptions if the MPI.ERRORS_RETURN error handler is set:

    MPI.COMM_WORLD.setErrhandler(MPI.ERRORS_RETURN);

If you add this statement to your program, it will show the line
where it breaks, instead of just crashing in case of an error.
Error-handling code can be separated from main application code by
means of try-catch blocks, for instance:

    try
    {
        File file = new File(MPI.COMM_SELF, "filename", MPI.MODE_RDONLY);
    }
    catch(MPIException ex)
    {
        System.err.println("Error Message: "+ ex.getMessage());
        System.err.println("  Error Class: "+ ex.getErrorClass());
        ex.printStackTrace();
        System.exit(-1);
    }


----------------------------------------------------------------------------

How to specify buffers

In MPI primitives that require a buffer (either send or receive) the
Java API admits a Java array. Since Java arrays can be relocated by
the Java runtime environment, the MPI Java bindings need to make a
copy of the contents of the array to a temporary buffer, then pass the
pointer to this buffer to the underlying C implementation. From the
practical point of view, this implies an overhead associated to all
buffers that are represented by Java arrays. The overhead is small
for small buffers but increases for large arrays.

There is a pool of temporary buffers with a default capacity of 64K.
If a temporary buffer of 64K or less is needed, then the buffer will
be obtained from the pool. But if the buffer is larger, then it will
be necessary to allocate the buffer and free it later.

The default capacity of pool buffers can be modified with an 'mca'
parameter:

    mpirun --mca mpi_java_eager size ...

Where 'size' is the number of bytes, or kilobytes if it ends with 'k',
or megabytes if it ends with 'm'.

An alternative is to use "direct buffers" provided by standard
classes available in the Java SDK such as ByteBuffer. For convenience
we provide a few static methods "new[Type]Buffer" in the MPI class
to create direct buffers for a number of basic datatypes. Elements
of the direct buffer can be accessed with methods put() and get(),
and the number of elements in the buffer can be obtained with the
method capacity(). This example illustrates its use:

    int myself = MPI.COMM_WORLD.getRank();
    int tasks  = MPI.COMM_WORLD.getSize();

    IntBuffer in  = MPI.newIntBuffer(MAXLEN * tasks),
              out = MPI.newIntBuffer(MAXLEN);

    for(int i = 0; i < MAXLEN; i++)
        out.put(i, myself);      // fill the buffer with the rank

    Request request = MPI.COMM_WORLD.iAllGather(
                      out, MAXLEN, MPI.INT, in, MAXLEN, MPI.INT);
    request.waitFor();
    request.free();

    for(int i = 0; i < tasks; i++)
    {
        for(int k = 0; k < MAXLEN; k++)
        {
            if(in.get(k + i * MAXLEN) != i)
                throw new AssertionError("Unexpected value");
        }
    }

Direct buffers are available for: BYTE, CHAR, SHORT, INT, LONG,
FLOAT, and DOUBLE. There is no direct buffer for booleans.

Direct buffers are not a replacement for arrays, because they have
higher allocation and deallocation costs than arrays. In some
cases arrays will be a better choice. You can easily convert a
buffer into an array and vice versa.

All non-blocking methods must use direct buffers and only
blocking methods can choose between arrays and direct buffers.

The above example also illustrates that it is necessary to call
the free() method on objects whose class implements the Freeable
interface. Otherwise a memory leak is produced.

----------------------------------------------------------------------------

Specifying offsets in buffers

In a C program, it is common to specify an offset in a array with
"&array[i]" or "array+i", for instance to send data starting from
a given positon in the array. The equivalent form in the Java bindings
is to "slice()" the buffer to start at an offset. Making a "slice()"
on a buffer is only necessary, when the offset is not zero. Slices
work for both arrays and direct buffers.

    import static mpi.MPI.slice;
    ...
    int numbers[] = new int[SIZE];
    ...
    MPI.COMM_WORLD.send(slice(numbers, offset), count, MPI.INT, 1, 0);

----------------------------------------------------------------------------

If you have any problems, or find any bugs, please feel free to report
them to Open MPI user's mailing list (see
http://www.open-mpi.org/community/lists/ompi.php).
