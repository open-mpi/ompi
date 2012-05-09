#
# Copyright (c) 2012      Los Alamos National Security, Inc.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.man-page-rules

man_pages = hdfsalloc.1
EXTRA_DIST = $(man_pages:.1=.1in)

bin_PROGRAMS = 

# Automake doesn't directly know about Java source files, and we compile
# them via *.java below (ick!).  So we just list them here in EXTRA_DIST
# so that they get picked up by "make dist".
EXTRA_DIST += HDFSFileFinder.java

if OMPI_INSTALL_BINARIES
if !ORTE_DISABLE_FULL_SUPPORT

# Only do this stuff if we want Hadoop support
if ORTE_WANT_HADOOP_SUPPORT

nodist_man_MANS = $(man_pages)

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(nodist_man_MANS): $(top_builddir)/opal/include/opal_config.h



# A little verbosity magic; "make" will show the terse output.  "make
# V=1" will show the actual commands used (just like the other
# Automake-generated compilation/linker rules).
ORTE_V_JAVAC = $(orte__v_JAVAC_$(V))
orte__v_JAVAC_ = $(orte__v_JAVAC_$(AM_DEFAULT_VERBOSITY))
orte__v_JAVAC_0 = @echo "  JAVAC " `basename $@`;

ORTE_V_JAR = $(orte__v_JAR_$(V))
orte__v_JAR_ = $(orte__v_JAR_$(AM_DEFAULT_VERBOSITY))
orte__v_JAR_0 = @echo "  JAR  " `basename $@`;

bin_PROGRAMS += HDFSFileFinder.class

if ORTE_HAVE_HADOOP_SERIES1
hadoop_jars = .:$(HADOOP_HOME)/*:$(HADOOP_HOME)/lib/*
else
hadoop_jars = .:$(HADOOP_COMMON_HOME)/share/hadoop/common/*:$(HADOOP_COMMON_HOME)/share/hadoop/common/lib/*:$(HADOOP_HDFS_HOME)/share/hadoop/hdfs/*:$(HADOOP_HDFS_HOME)/share/hadoop/hdfs/lib/*:$(HADOOP_MAPRED_HOME)/share/hadoop/mapreduce/*:$(HADOOP_MAPRED_HOME)/share/hadoop/mapreduce/lib/*:$(YARN_HOME)/share/hadoop/httpfs/tomcat/lib/*:$(YARN_HOME)/share/hadoop/httpfs/tomcat/webapps/webhdfs/WEB-INF/lib/*
endif

HDFSFileFinder.class: HDFSFileFinder.java
	$(JAVAC) -d . -classpath $(hadoop_jars) $(top_srcdir)/orte/tools/allocators/HDFSFileFinder.java

install-exec-hook:
	test -z "$(bindir)" || $(mkdir_p) "$(DESTDIR)$(bindir)"
	(cp hdfsalloc.pl $(DESTDIR)$(bindir))
	(cd $(DESTDIR)$(bindir); chmod +x hdfsalloc.pl; rm -f hdfsalloc; $(LN_S) hdfsalloc.pl hdfsalloc)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/hdfsalloc.pl \
	$(DESTDIR)$(bindir)/hdfsalloc

# Generate the .jar file
hdfsalloc.jar: HDFSFileFinder.class
	$(ORTE_V_JAR) $(JAR) cf hdfsalloc.jar HDFSFileFinder.class

# Install the jar file into libdir.  Use the DATA Automake primary,
# because Automake will complain if you try to use LIBRARIES with a
# filename that doesn't fit the lib<foo>.* format.  Also use an
# indirection to get to the libdir -- Automake does not allow putting
# libdir for the DATA primary.
javadir = $(libdir)
java_DATA = hdfsalloc.jar

# Clean up all the things that this Makefile.am generates.
CLEANFILES += HDFSFileFinder.class hdfsalloc.jar

endif # ORTE_WANT_HADOOP_SUPPORT
endif # !ORTE_DISABLE_FULL_SUPPORT
endif # OMPI_INSTALL_BINARIES

distclean-local:
	rm -f $(man_pages)
