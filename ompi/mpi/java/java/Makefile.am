# -*- makefile -*-
#
# Copyright (c) 2011	Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# These are the source files.  However, Automake doesn't directly know
# about them, and we compile them via *.java below (ick!).  So we just
# list them here in EXTRA_DIST so that they get picked up by "make
# dist".
JAVA_SRC_FILES = \
        Cartcomm.java     \
        CartParms.java    \
        Comm.java         \
        Datatype.java     \
        Errhandler.java   \
        Freeable.java     \
        Graphcomm.java    \
        GraphParms.java   \
        Group.java        \
        Intercomm.java    \
        Intracomm.java    \
        Maxloc.java       \
        Minloc.java       \
        MPI.java          \
        MPIException.java \
        Op.java           \
        Prequest.java     \
        Request.java      \
        ShiftParms.java   \
        Status.java       \
        User_function.java 

JAVA_CLASS_FILES = $(JAVA_SRC_FILES:%.java=mpi/%.class)
EXTRA_DIST = $(JAVA_SRC_FILES)

# Only do this stuff if we want the Java bindings
if OMPI_WANT_JAVA_BINDINGS

# These files get generated.  They have a 1:1 correspondence to .java
# files, but there is not a .h file for every .java file.  That's why
# we have a specific list of files here, as opposed to deriving them
# from JAVA_SRC_FILES.
JAVA_H = \
        mpi_MPI.h          \
        mpi_CartParms.h    \
        mpi_Cartcomm.h     \
        mpi_Comm.h         \
        mpi_Datatype.h     \
        mpi_Errhandler.h   \
        mpi_GraphParms.h   \
        mpi_Graphcomm.h    \
        mpi_Group.h        \
        mpi_Intercomm.h    \
        mpi_Intracomm.h    \
        mpi_Op.h           \
        mpi_Prequest.h     \
        mpi_Request.h      \
        mpi_ShiftParms.h   \
        mpi_Status.h       \
        mpi_User_function.h 

# A little verbosity magic; "make" will show the terse output.  "make
# V=1" will show the actual commands used (just like the other
# Automake-generated compilation/linker rules).
OMPI_V_JAVAC = $(ompi__v_JAVAC_$(V))
ompi__v_JAVAC_ = $(ompi__v_JAVAC_$(AM_DEFAULT_VERBOSITY))
ompi__v_JAVAC_0 = @echo "  JAVAC " `basename $@`;

OMPI_V_JAVAH = $(ompi__v_JAVAH_$(V))
ompi__v_JAVAH_ = $(ompi__v_JAVAH_$(AM_DEFAULT_VERBOSITY))
ompi__v_JAVAH_0 = @echo "  JAVAH " `basename $@`;

OMPI_V_JAR = $(ompi__v_JAR_$(V))
ompi__v_JAR_ = $(ompi__v_JAR_$(AM_DEFAULT_VERBOSITY))
ompi__v_JAR_0 = @echo "  JAR  " `basename $@`;

OMPI_V_MKDIR = $(ompi__v_MKDIR_$(V))
ompi__v_MKDIR_ = $(ompi__v_MKDIR_$(AM_DEFAULT_VERBOSITY))
ompi__v_MKDIR_0 = @echo "  MKDIR " $@;

# All the .java files seem to have circular references, such that I
# can't figure out a linear order in which to compile them
# sequentially that does not generate dependency errors.  Hence, the
# only way I can figure out how to compile them is via *.java -- this
# could well be due to my own misunderstanding of Java or the
# compiler.  Shrug.
#
# So instead of listing all the .class files, just use mpi/MPI.class
# as a token class file for both the rule and all the dependencies
# below.
#
# Note too, that all of them will be recompiled if any of them change,
# since Automake doesn't know how to automatically generate
# dependencies for Java source files.  So I made the token stamp1.class
# file dependent upon *all* the .java source files.
mpi/MPI.class: $(JAVA_SRC_FILES)
	$(OMPI_V_JAVAC) CLASSPATH=. ; \
	export CLASSPATH ; \
	$(JAVAC) -d . $(top_srcdir)/ompi/mpi/java/java/*.java

# Similar to above, all the generated .h files are dependent upon the
# token mpi/MPI.class file.  Hence, all the classes will be generated
# first, then we'll individuall generate each of the .h files.
$(JAVA_H): mpi/MPI.class
	$(OMPI_V_JAVAH) sourcename=mpi.`echo $@ | sed -e s/^mpi_// -e s/.h$$//`; \
	CLASSPATH=. ; \
	export CLASSPATH ; \
	$(JAVAH) -d . -jni $$sourcename

# Generate the .jar file from all the class files.  Use the token
# mpi/MPI.class file (see explanation above).  If we try to use
# JAVA_CLASS_FILES in the dependency here, make will complain that it
# doesn't know how to build all the files.
mpi.jar: mpi/MPI.class
	$(OMPI_V_JAR) $(JAR) cf mpi.jar $(JAVA_CLASS_FILES)

# Install the jar file into libdir.  Use the DATA Automake primary,
# because Automake will complain if you try to use LIBRARIES with a
# filename that doesn't fit the lib<foo>.* format.  Also use an
# indirection to get to the libdir -- Automake does not allow putting
# libdir for the DATA primary.
javadir = $(libdir)
java_DATA = mpi.jar

# List all the header files in BUILT_SOURCES so that Automake's "all"
# target will build them.  This will also force the building of the
# JAVA_CLASS_FILES.
BUILT_SOURCES = $(JAVA_H)

# Clean up all the things that this Makefile.am generates.  The
# generated .class files are all within the "mpi" subdirectory.
CLEANFILES += -rf mpi/* $(JAVA_H)

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompihdir = $(includedir)/openmpi/$(subdir)
nobase_nodist_ompih_HEADERS = $(JAVA_H)
endif

endif
