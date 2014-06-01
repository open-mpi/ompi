#
# We generate three general things in this directory:
#
# 1. *.java files get compiled into shmem/*.class files.
# 2. The shmem/*.class files are then assembled into an shmem.jar file.
# 3. The shmem/*.class files are analyzed to make *.h JNI files.
#

# These are the Java source files.  However, Automake doesn't directly
# know about them, and we compile them via *.java below (ick!).  So we
# just list them here in EXTRA_DIST so that they get picked up by
# "make dist".
JAVA_SRC_FILES = \
        Addr.java \
        Constant.java \
	PSync.java \
        ShMem.java \
        ShMemException.java

EXTRA_DIST = $(JAVA_SRC_FILES)

# Only do this stuff if we want the Java bindings
if OMPI_WANT_JAVA_BINDINGS

# These files get generated.  They have a 1:1 correspondence to .java
# files, but there is not a .h file for every .java file.  That's why
# we have a specific list of files here, as opposed to deriving them
# from JAVA_SRC_FILES.
JAVA_H = \
        shmem_Addr.h \
	shmem_Constant.h \
	shmem_PSync.h \
        shmem_ShMem.h

# A little verbosity magic; "make" will show the terse output.  "make
# V=1" will show the actual commands used (just like the other
# Automake-generated compilation/linker rules).
V=0
OMPI_V_JAVAC = $(ompi__v_JAVAC_$V)
ompi__v_JAVAC_ = $(ompi__v_JAVAC_$AM_DEFAULT_VERBOSITY)
ompi__v_JAVAC_0 = @echo "  JAVAC " `basename $@`;

OMPI_V_JAVAH = $(ompi__v_JAVAH_$V)
ompi__v_JAVAH_ = $(ompi__v_JAVAH_$AM_DEFAULT_VERBOSITY)
ompi__v_JAVAH_0 = @echo "  JAVAH " `basename $@`;

OMPI_V_JAR = $(ompi__v_JAR_$V)
ompi__v_JAR_ = $(ompi__v_JAR_$AM_DEFAULT_VERBOSITY)
ompi__v_JAR_0 = @echo "    JAR " `basename $@`;

OMPI_V_JAVADOC = $(ompi__v_JAVADOC_$V)
ompi__v_JAVADOC_ = $(ompi__v_JAVADOC_$AM_DEFAULT_VERBOSITY)
ompi__v_JAVADOC_0 = @echo "JAVADOC " `basename $@`;

OMPI_V_JAVADOC_QUIET = $(ompi__v_JAVADOC_QUIET_$V)
ompi__v_JAVADOC_QUIET_ = $(ompi__v_JAVADOC_QUIET_$AM_DEFAULT_VERBOSITY)
ompi__v_JAVADOC_QUIET_0 = -quiet

OMPI_V_MKDIR = $(ompi__v_MKDIR_$V)
ompi__v_MKDIR_ = $(ompi__v_MKDIR_$AM_DEFAULT_VERBOSITY)
ompi__v_MKDIR_0 = @echo "  MKDIR " $@;

# All the .java files seem to have circular references, such that I
# can't figure out a linear order in which to compile them
# sequentially that does not generate dependency errors.  Hence, the
# only way I can figure out how to compile them is via *.java -- this
# could well be due to my own misunderstanding of Java or the
# compiler.  Shrug.
#
# So instead of listing all the .class files, since the rule below
# will generate *all* the .class files simulanteously, just use
# shmem/ShMem.class as a token class file for both the rule and all the
# dependencies below.
#
# Note too, that all of them will be recompiled if any of them change,
# since Automake doesn't know how to automatically generate
# dependencies for Java source files.  So I made the token ShMem.class
# file dependent upon *all* the .java source files.
#
# Note that the javac compile will generate all the .class files in
# the "shmem" subdirectory, because that's the java package that they're
# in.  This, along with the fact that the .java files seem to have
# circular references, prevents us from using a .foo.bar: generic
# Makefile rule. :-(
shmem/ShMem.class: $(JAVA_SRC_FILES)
	$(OMPI_V_JAVAC) CLASSPATH=. ; \
	export CLASSPATH ; \
	$(JAVAC) -d . $(OSHMEM_TOP_SRCDIR)/oshmem/shmem/java/java/*.java

# Similar to above, all the generated .h files are dependent upon the
# token shmem/ShMem.class file.  Hence, all the classes will be generated
# first, then we'll individually generate each of the .h files.
$(JAVA_H): shmem/ShMem.class
	$(OMPI_V_JAVAH) sourcename=shmem.`echo $@ | sed -e s/^shmem_// -e s/.h$$//`; \
	CLASSPATH=. ; \
	export CLASSPATH ; \
	$(JAVAH) -d . -jni $$sourcename

# Generate the .jar file from all the class files.  List shmem/ShMem.class
# as a dependency so that it fires the rule above that will generate
# *all* the shmem/*.class files.
shmem.jar: shmem/ShMem.class
	$(OMPI_V_JAR) $(JAR) cf shmem.jar shmem/*.class

# Install the jar file into libdir.  Use the DATA Automake primary,
# because Automake will complain if you try to use LIBRARIES with a
# filename that doesn't fit the lib<foo>.* format.  Also use an
# indirection to get to the libdir -- Automake does not allow putting
# libdir for the DATA primary.
javadir = $(libdir)
java_DATA = shmem.jar

# List all the header files in BUILT_SOURCES so that Automake's "all"
# target will build them.  This will also force the building of the
# shmem/*.class files (for the jar file).
BUILT_SOURCES = $(JAVA_H) doc

# Convenience for building Javadoc docs
jdoc: doc

# Make the "doc" target (and subdir) dependent upon shmem/ShMem.class; if
# shmem.jar is ever rebuilt, then also make the docs eligible to be
# rebuilt.
doc: shmem/ShMem.class
	$(OMPI_V_JAVADOC) javadoc $(OMPI_V_JAVADOC_QUIET) -d doc $(srcdir)/*.java
	@touch doc

jdoc-install: doc
	-$(MKDIR_P) $(DESTDIR)$(docdir)/javadoc-oshmem
	cp -rp doc/* $(DESTDIR)$(docdir)/javadoc-oshmem

jdoc-uninstall:
	-rm -rf $(DESTDIR)$(docdir)/../oshmem/javadoc

install-data-hook: jdoc-install
uninstall-local: jdoc-uninstall

# Clean up all the things that this Makefile.am generates.
CLEANFILES += $(JAVA_H) shmem.jar

# Can only put *files* in CLEANFILES; need to remove the generated doc
# and shmem directories separately.
clean-local:
	-rm -rf doc shmem

# Conditionally install the header files
if WANT_INSTALL_HEADERS
oshmemdir = $(oshmemincludedir)/$(subdir)
oshmem_HEADERS = $(headers)
endif

endif
