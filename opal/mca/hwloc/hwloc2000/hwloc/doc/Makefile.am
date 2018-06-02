# Copyright © 2009-2018 Inria.  All rights reserved.
# Copyright © 2009-2013 Université Bordeaux
# Copyright © 2009-2016 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.

AM_CPPFLAGS = $(HWLOC_CPPFLAGS)

DOCDIR = $(HWLOC_top_builddir)/doc
DOX_CONFIG = $(HWLOC_top_srcdir)/doc/doxygen.cfg

DOX_DIR = doxygen-doc
DOX_HTML_DIR = $(DOX_DIR)/html
DOX_MAN_DIR = $(DOX_DIR)/man
DOX_LATEX_DIR = $(DOX_DIR)/latex
DOX_A4PDF = doxygen-doc/$(PACKAGE)-a4.pdf
DOX_LETTERPDF = doxygen-doc/$(PACKAGE)-letter.pdf
DOX_HTML_README = $(DOX_HTML_DIR)/readme.tmp.html
DOX_TAG = $(DOX_DIR)/$(PACKAGE).tag

#
# The goal is that Doxygen output (i.e., the documentation) is
# included in release tarballs; there is no need to build anything
# from tarballs.  Developers will need to build the docs the first
# time they build after a checkout (or if something in the docs
# changes, etc.).
#

# images already available in repository
PREBUILT_IMAGES = images/dudley.png images/emmett.png images/hagrid.png \
        images/ppc64-with-smt.png images/ppc64-without-smt.png images/ppc64-full-with-smt.png \
        images/devel09-pci.png \
        images/netloc_design.png images/netloc_draw.png
# images built from repository before inclusion in make dist
BUILT_IMAGES = images/diagram.png images/diagram.eps

EXTRA_DIST = \
        main.doxy \
        hwloc.doxy \
        netloc.doxy \
        README.sed \
        doxygen.css \
        doxygen.cfg \
        images/diagram.fig \
        www.open-mpi.org.cfg \
        www.open-mpi.org-css.inc \
        www.open-mpi.org-footer.inc \
        www.open-mpi.org-header.inc \
        $(PREBUILT_IMAGES) \
        $(BUILT_IMAGES)



if HWLOC_BUILD_DOXYGEN

#
# Files where the doxygen inputs live (i.e., dependencies).  Make the
# generated tagfile depend on these files, which will force them to be
# regenerated (i.e., re-run doxygen) whenever any of these files
# change.
#

hwloc_include_dir = $(HWLOC_top_srcdir)/include

dox_inputs = $(DOX_CONFIG) \
       $(srcdir)/main.doxy \
       $(srcdir)/hwloc.doxy \
       $(hwloc_include_dir)/hwloc.h \
       $(hwloc_include_dir)/hwloc/helper.h \
       $(hwloc_include_dir)/hwloc/bitmap.h \
       $(hwloc_include_dir)/hwloc/export.h \
       $(hwloc_include_dir)/hwloc/distances.h \
       $(hwloc_include_dir)/hwloc/diff.h \
       $(hwloc_include_dir)/hwloc/shmem.h \
       $(hwloc_include_dir)/hwloc/plugins.h \
       $(hwloc_include_dir)/hwloc/glibc-sched.h \
       $(hwloc_include_dir)/hwloc/linux.h \
       $(hwloc_include_dir)/hwloc/linux-libnuma.h \
       $(hwloc_include_dir)/hwloc/opencl.h \
       $(hwloc_include_dir)/hwloc/cuda.h \
       $(hwloc_include_dir)/hwloc/cudart.h \
       $(hwloc_include_dir)/hwloc/nvml.h \
       $(hwloc_include_dir)/hwloc/gl.h \
       $(hwloc_include_dir)/hwloc/intel-mic.h \
       $(hwloc_include_dir)/hwloc/openfabrics-verbs.h \
       $(srcdir)/netloc.doxy \
       $(hwloc_include_dir)/netloc.h

#
# Create the images that we need for the PDF output and the HTML
# output.  There is not an easy way to check if the output of
# AC_PATH_PROG found something in configure.ac (!), so we have to put
# a run-time check here to see if fig2dev was found.  :-(
#
# Note that BUILD_DOXYGEN will automatically be false if we're not
# building standalone.
#

BUILT_SOURCES = $(BUILT_IMAGES)

images/diagram.png: $(srcdir)/images/diagram.fig
	$(MKDIR_P) images
	@if test "x$(FIG2DEV)" = "x"; then \
	    echo "ERROR: Cannot find the 'fig2dev' executable -- cannot make $@"; \
	    exit 1; \
	fi
	$(FIG2DEV) -m 1.5 -L png $< $@

images/diagram.eps: $(srcdir)/images/diagram.fig
	$(MKDIR_P) images
	@if test "x$(FIG2DEV)" = "x"; then \
	    echo "ERROR: Cannot find the 'fig2dev' executable -- cannot make $@"; \
	    exit 1; \
	fi
	$(FIG2DEV) -L eps $< $@

#
# Rules for running doxygen.  It depends on the built images and the
# dox_inputs.
# Remove useless manpages, they have too long filenames anyway because
# of nested structurre/union declarations.
#

#
# Note that we want to use our own doxygen.css file; not the one that
# doxygen installs in the HTML directory.  So manually copy it over.
# Be a little clever: only copy the doxygen.css file over if it exists
# in DOX_HTML_DIR (which is in the build tree).  If the html tree
# doesn't exist in the build tree, then we're using an html tree in
# the source tree, and we don't need to copy over the doxygen.css
# because we didn't build the html tree (e.g., the html tree came
# pre-bundled in a tarball).
#

$(DOX_TAG): $(BUILT_SOURCES) $(dox_inputs) $(PREBUILT_IMAGES)
	rm -fr $(DOX_DIR)
	$(DOXYGEN) $(DOX_CONFIG)
	sed -i -e 's@\\hyperlink{a[[:digit:]][[:digit:]]*_source}{@\\textbf{@g' $(DOX_DIR)/latex/*.tex
	@echo "Work-around spurious leading _ in doxygen filenames..."
	-(cd $(DOX_DIR)/man/man3 ; \
	for i in _hwloc* ; do \
		[ ! -f $$i ] || mv $$i $${i#_} ; \
	done)
	@echo "Removing useless manpages..."
	@mkdir $(DOX_DIR)/man.tmp
	@mv $(DOX_MAN_DIR)/man3/hwloc* $(DOX_MAN_DIR)/man3/HWLOC* $(DOX_DIR)/man.tmp/
	@rm -rf $(DOX_MAN_DIR)/man3
	@mv $(DOX_DIR)/man.tmp $(DOX_MAN_DIR)/man3
	@echo "Copying custom doxygen.css..."
	if test -d $(DOX_HTML_DIR) -a -f $(DOX_HTML_DIR)/doxygen.css; then \
	    cp -f $(srcdir)/doxygen.css $(DOX_HTML_DIR); \
	fi

#
# Rules for building the PDF
#

# The Doxygen config is set to generate a4 latex -- no transformation
# is necessary.
$(DOX_A4PDF): $(DOX_TAG)
	cd $(DOX_LATEX_DIR); \
	rm -f *.aux *.toc *.idx *.ind *.ilg *.log *.out; \
	cp refman.tex a4-refman.tex; \
	$(PDFLATEX) a4-refman.tex; \
	$(MAKEINDEX) a4-refman.idx; \
	$(PDFLATEX) a4-refman.tex; \
	done=0; repeat=5; \
	while test $$done = 0 -a $$repeat -gt 0; do \
           if $(EGREP) 'Rerun (LaTeX|to get cross-references right)' a4-refman.log > /dev/null 2>&1; then \
	       $(PDFLATEX) a4-refman.tex; \
	       repeat=`expr $$repeat - 1`; \
	   else \
	       done=1; \
	   fi; \
	done; \
	mv a4-refman.pdf $(DOCDIR)/$(DOX_A4PDF)

# The Doxygen config is set to generate a4 latex -- slightly transform
# to make suitable for US letter.
$(DOX_LETTERPDF): $(DOX_TAG)
	cd $(DOX_LATEX_DIR); \
	rm -f *.aux *.toc *.idx *.ind *.ilg *.log *.out; \
	$(SED) -e 's/a4paper/letterpaper/g' -e 's/\\usepackage{a4wide}//' refman.tex > letter-refman.tex; \
	$(PDFLATEX) letter-refman.tex; \
	$(MAKEINDEX) letter-refman.idx; \
	$(PDFLATEX) letter-refman.tex; \
	done=0; repeat=5; \
	while test $$done = 0 -a $$repeat -gt 0; do \
           if $(EGREP) 'Rerun (LaTeX|to get cross-references right)' letter-refman.log > /dev/null 2>&1; then \
	       $(PDFLATEX) letter-refman.tex; \
	       repeat=`expr $$repeat - 1`; \
	   else \
	       done=1; \
	   fi; \
	done; \
	mv letter-refman.pdf $(DOCDIR)/$(DOX_LETTERPDF)

doc: $(DOX_TAG) $(DOX_LETTERPDF) $(DOX_A4PDF)

else !HWLOC_BUILD_DOXYGEN

# When we don't have doxygen, nothing to do for a normal build
doc:

if !HWLOC_INSTALL_DOXYGEN
# But if the user tries to "make dist" and doesn't have all the right
# tools, and prebuilt doc aren't available, fail with a helpful error
$(BUILT_IMAGES) $(DOX_A4PDF) $(DOX_LETTERPDF):
	@echo " "
	@echo "*** ERROR: You do not have all the documentation generation tools"
	@echo "*** that hwloc needs.  Check the output from when you ran 'configure'"
	@echo "*** (in section \"Configuring hwloc documentation\") to see what tools"
	@echo "*** you are missing."
	@echo "***"
	@echo "*** 'make' will now abort with an error."
	@echo " "
	@exit 1
endif !HWLOC_INSTALL_DOXYGEN

endif !HWLOC_BUILD_DOXYGEN
# there are also a bunch of doxygen-build-dependencies later in this file



if HWLOC_INSTALL_DOXYGEN

#
# Un/install the generated PDF and man pages (just like BUILD_DOXYGEN,
# INSTALL_DOXYGEN will automatically be false if we're not building in
# standalone mode).
#

pdf_DATA = $(DOX_A4PDF) $(DOX_LETTERPDF)

#
# Install the HWLOC_* and hwloc_* man pages.  It would be great to
# figure out how to not have to list every installable man page here
# in the Makefile.am...  :-(
#

# We have so many man pages that if we put them all in man3_MANS, we
# exceed the maximum command line length on some systems (during "make
# install").  :-( So we have to split them up so that the total
# command line length will be shorter.  It seemed logical to split
# them by small-ish groups.  Put at least one group in man3_MANS so
# that the proper $mandir/man/man3 is created, and then put the rest
# in _DATA suffix names, because AM won't allow us to do things like
# man3_extra_MANS = ...group...

# When adding a new _DATA line below, remember to add it to the
# list of dependencies too.

man3_MANS = \
        $(DOX_MAN_DIR)/man3/hwlocality_api_version.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_API_VERSION.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_api_version.3

man3_object_setsdir = $(man3dir)
man3_object_sets_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_object_sets.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_const_bitmap_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_const_cpuset_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nodeset_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_const_nodeset_t.3

man3_object_typesdir = $(man3dir)
man3_object_types_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_object_types.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_BRIDGE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_CORE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_GROUP.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L1CACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L2CACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L3CACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L4CACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L5CACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L1ICACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L2ICACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_L3ICACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_MACHINE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_MISC.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_NUMANODE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_OS_DEVICE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_PU.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_PCI_DEVICE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_PACKAGE.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_t.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_CACHE_UNIFIED.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_CACHE_DATA.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_CACHE_INSTRUCTION.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_cache_type_e.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_cache_type_t.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_BRIDGE_HOST.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_BRIDGE_PCI.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_bridge_type_e.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_bridge_type_t.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_OSDEV_BLOCK.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_OSDEV_DMA.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_OSDEV_GPU.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_OSDEV_NETWORK.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_OSDEV_OPENFABRICS.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_OSDEV_COPROC.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_osdev_type_e.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_osdev_type_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_compare_types.3 \
        $(DOX_MAN_DIR)/man3/hwloc_compare_types_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_UNORDERED.3

man3_objectsdir = $(man3dir)
man3_objects_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_objects.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_numanode_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_numanode_attr_s_hwloc_memory_page_type_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_bridge_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_cache_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_group_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_osdev_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_pcidev_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_info_s.3

man3_creationdir = $(man3dir)
man3_creation_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_creation.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_init.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_load.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_destroy.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_dup.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_abi_check.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_check.3

man3_levelsdir = $(man3dir)
man3_levels_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_levels.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_type_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_type_depth_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_DEPTH_MULTIPLE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_DEPTH_UNKNOWN.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_DEPTH_BRIDGE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_DEPTH_PCI_DEVICE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_DEPTH_OS_DEVICE.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_memory_parents_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_type_or_above_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_type_or_below_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_depth_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_root_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_by_type.3

man3_object_stringsdir = $(man3dir)
man3_object_strings_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_object_strings.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_string.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_type_sscanf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_type_sscanf_as_depth.3

man3_object_info_attrsdir = $(man3dir)
man3_object_info_attrs_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_info_attr.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_get_info_by_name.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_add_info.3

man3_cpubindingdir = $(man3dir)
man3_cpubinding_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_cpubinding.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_CPUBIND_NOMEMBIND.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_CPUBIND_PROCESS.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_CPUBIND_STRICT.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_CPUBIND_THREAD.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpubind_flags_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_proc_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_proc_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_thread_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_thread_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_proc_last_cpu_location.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_last_cpu_location.3

man3_membindingdir = $(man3dir)
man3_membinding_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_membinding.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_BIND.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_DEFAULT.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_FIRSTTOUCH.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_INTERLEAVE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_MIGRATE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_MIXED.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_NEXTTOUCH.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_NOCPUBIND.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_BYNODESET.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_PROCESS.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_STRICT.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_MEMBIND_THREAD.3 \
        $(DOX_MAN_DIR)/man3/hwloc_membind_flags_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_membind_policy_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_membind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_membind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_proc_membind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_proc_membind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_area_membind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_area_membind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_area_memlocation.3 \
        $(DOX_MAN_DIR)/man3/hwloc_alloc.3 \
        $(DOX_MAN_DIR)/man3/hwloc_alloc_membind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_alloc_membind_policy.3 \
        $(DOX_MAN_DIR)/man3/hwloc_free.3

man3_setsourcedir = $(man3dir)
man3_setsource_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_setsource.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_pid.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_synthetic.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_xml.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_xmlbuffer.3

man3_configurationdir = $(man3dir)
man3_configuration_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_configuration.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_FLAG_THISSYSTEM_ALLOWED_RESOURCES.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_flags_e.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_flags.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_flags.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_is_thissystem.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_discovery_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_cpubind_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_membind_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_type_filter_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_FILTER_KEEP_ALL.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_FILTER_KEEP_NONE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_FILTER_KEEP_STRUCTURE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_FILTER_KEEP_IMPORTANT.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_type_filter.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_all_types_filter.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_cache_types_filter.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_icache_types_filter.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_io_types_filter.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_type_filter.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_userdata.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_userdata.3

man3_tinkerdir = $(man3dir)
man3_tinker_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_tinker.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_RESTRICT_FLAG_REMOVE_CPULESS.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_RESTRICT_FLAG_ADAPT_IO.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_RESTRICT_FLAG_ADAPT_MISC.3 \
        $(DOX_MAN_DIR)/man3/hwloc_restrict_flags_e.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_restrict.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_insert_misc_object.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_alloc_group_object.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_insert_group_object.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_add_other_obj_sets.3

man3_xmlexportdir = $(man3dir)
man3_xmlexport_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_xmlexport.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_export_xml.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_export_xmlbuffer.3 \
        $(DOX_MAN_DIR)/man3/hwloc_free_xmlbuffer.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_userdata_export_callback.3 \
        $(DOX_MAN_DIR)/man3/hwloc_export_obj_userdata.3 \
        $(DOX_MAN_DIR)/man3/hwloc_export_obj_userdata_base64.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_userdata_import_callback.3

man3_syntheticexportdir = $(man3dir)
man3_syntheticexport_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_syntheticexport.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_export_synthetic.3

man3_shmemdir = $(man3dir)
man3_shmem_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_shmem.3 \
        $(DOX_MAN_DIR)/man3/hwloc_shmem_topology_get_length.3 \
        $(DOX_MAN_DIR)/man3/hwloc_shmem_topology_write.3 \
        $(DOX_MAN_DIR)/man3/hwloc_shmem_topology_adopt.3

man3_bitmapdir = $(man3dir)
man3_bitmap_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_bitmap.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_allbut.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_alloc.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_alloc_full.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_and.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_andnot.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_asprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_clr.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_clr_range.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_compare.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_compare_first.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_copy.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_dup.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_fill.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_first.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_foreach_begin.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_foreach_end.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_free.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_from_ith_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_from_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_intersects.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_isequal.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_isfull.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_isincluded.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_isset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_iszero.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_last.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_list_asprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_list_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_list_sscanf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_next.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_not.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_only.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_or.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_set.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_set_ith_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_set_range.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_singlify.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_sscanf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_taskset_asprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_taskset_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_taskset_sscanf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_to_ith_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_to_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_weight.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_xor.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bitmap_zero.3

man3_helper_find_insidedir = $(man3dir)
man3_helper_find_inside_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_inside.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_first_largest_obj_inside_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_largest_objs_inside_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_inside_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_inside_cpuset_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_inside_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_inside_cpuset_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_inside_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_inside_cpuset_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_index_inside_cpuset.3

man3_helper_find_coveringdir = $(man3dir)
man3_helper_find_covering_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_covering.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_child_covering_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_covering_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_covering_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_covering_cpuset_by_type.3

man3_helper_ancestorsdir = $(man3dir)
man3_helper_ancestors_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_ancestors.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_ancestor_obj_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_ancestor_obj_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_common_ancestor_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_is_in_subtree.3

man3_helper_find_typesdir = $(mandir)
man3_helper_find_types_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_types.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_is_normal.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_is_memory.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_is_io.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_is_cache.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_is_dcache.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_is_icache.3

man3_helper_find_cachedir = $(man3dir)
man3_helper_find_cache_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_cache.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_cache_type_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_cache_covering_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_shared_cache_covering_obj.3

man3_helper_find_miscdir = $(man3dir)
man3_helper_find_misc_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_misc.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_pu_obj_by_os_index.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_child.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_closest_objs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_below_array_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_below_by_type.3

man3_helper_distributedir = $(man3dir)
man3_helper_distribute_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_distribute.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distrib_flags_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_DISTRIB_FLAG_REVERSE.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distrib.3

man3_helper_topology_setsdir = $(man3dir)
man3_helper_topology_sets_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_topology_sets.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_allowed_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_complete_nodeset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_topology_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_allowed_nodeset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_complete_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_topology_nodeset.3

man3_helper_nodeset_convertdir = $(man3dir)
man3_helper_nodeset_convert_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_nodeset_convert.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_nodeset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_nodeset.3

man3_helper_distancesdir = $(man3dir)
man3_helper_distances_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_distances_get.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_kind_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_DISTANCES_KIND_FROM_OS.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_DISTANCES_KIND_FROM_USER.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_DISTANCES_KIND_MEANS_LATENCY.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_DISTANCES_KIND_MEANS_BANDWIDTH.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_get.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_get_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_get_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_release.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_distances_consult.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_obj_index.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_obj_pair_values.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_distances_add.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_add_flag_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_DISTANCES_ADD_FLAG_GROUP.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_DISTANCES_ADD_FLAG_GROUP_INACCURATE.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_add.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_remove.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_remove_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distances_remove_by_type.3

man3_helper_advanced_iodir = $(man3dir)
man3_helper_advanced_io_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_advanced_io.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_non_io_ancestor_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_pcidev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_pcidev_by_busid.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_pcidev_by_busidstring.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_osdev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_bridge.3 \
        $(DOX_MAN_DIR)/man3/hwloc_bridge_covers_pcibus.3

man3_diffdir = $(man3dir)
man3_diff_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_diff.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_obj_attr_type_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_NAME.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_obj_attr_u.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_type_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_DIFF_OBJ_ATTR.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_u.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_build.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_apply_flags_e.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_apply.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_destroy.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_load_xml.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_export_xml.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_load_xmlbuffer.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_diff_export_xmlbuffer.3

man3_cudadir = $(man3dir)
man3_cuda_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_opencl.3 \
        $(DOX_MAN_DIR)/man3/hwloc_opencl_get_device_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_opencl_get_device_osdev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_opencl_get_device_osdev_by_index.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_cuda.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cuda_get_device_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cuda_get_device_pci_ids.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cuda_get_device_pcidev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cuda_get_device_osdev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cuda_get_device_osdev_by_index.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_cudart.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cudart_get_device_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cudart_get_device_pci_ids.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cudart_get_device_pcidev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cudart_get_device_osdev_by_index.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_nvml.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nvml_get_device_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nvml_get_device_osdev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nvml_get_device_osdev_by_index.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_gl.3 \
        $(DOX_MAN_DIR)/man3/hwloc_gl_get_display_osdev_by_port_device.3 \
        $(DOX_MAN_DIR)/man3/hwloc_gl_get_display_osdev_by_name.3 \
        $(DOX_MAN_DIR)/man3/hwloc_gl_get_display_by_osdev.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_intel_mic.3 \
        $(DOX_MAN_DIR)/man3/hwloc_intel_mic_get_device_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_intel_mic_get_device_osdev_by_index.3

man3_glibc_scheddir = $(man3dir)
man3_glibc_sched_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_glibc_sched.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_glibc_sched_affinity.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_glibc_sched_affinity.3

man3_linuxdir = $(man3dir)
man3_linux_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_linux.3 \
        $(DOX_MAN_DIR)/man3/hwloc_linux_set_tid_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_linux_get_tid_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_linux_get_tid_last_cpu_location.3 \
        $(DOX_MAN_DIR)/man3/hwloc_linux_read_path_as_cpumask.3

man3_linux_libnumadir = $(man3dir)
man3_linux_libnuma_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nodeset_from_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nodeset_to_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_linux_libnuma_ulongs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_linux_libnuma_ulongs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nodeset_from_linux_libnuma_ulongs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_linux_libnuma_ulongs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_nodeset_to_linux_libnuma_ulongs.3

man3_openfabricsdir = $(man3dir)
man3_openfabrics_DATA = \
        $(DOX_MAN_DIR)/man3/hwlocality_openfabrics.3 \
        $(DOX_MAN_DIR)/man3/hwloc_ibv_get_device_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_ibv_get_device_osdev.3 \
        $(DOX_MAN_DIR)/man3/hwloc_ibv_get_device_osdev_by_name.3

# end of manpages(3) if HWLOC_BUILD_DOXYGEN

endif HWLOC_INSTALL_DOXYGEN



if HWLOC_BUILD_DOXYGEN

# Put in these rules to force the generation of the man pages and get
# all the dependencies right.  Additionally, this forces a
# serialization during parallel builds (e.g., "make -j X") to ensure
# that we don't get partial target errors (i.e., doxygen must be run
# *first* before we can try to process/install man pages, the HTML,
# run pdflatex, .etc.).

# The list below should match the list of _DATA lines above

$(man3_MANS): $(DOX_TAG)
$(man3_object_sets_DATA): $(DOX_TAG)
$(man3_object_types_DATA): $(DOX_TAG)
$(man3_objects_DATA): $(DOX_TAG)
$(man3_creation_DATA): $(DOX_TAG)
$(man3_configuration_DATA): $(DOX_TAG)
$(man3_levels_DATA): $(DOX_TAG)
$(man3_object_strings_DATA): $(DOX_TAG)
$(man3_object_info_attrs_DATA): $(DOX_TAG)
$(man3_cpubinding_DATA): $(DOX_TAG)
$(man3_membinding_DATA): $(DOX_TAG)
$(man3_tinker_DATA): $(DOX_TAG)
$(man3_setsource_DATA): $(DOX_TAG)
$(man3_xmlexport_DATA): $(DOX_TAG)
$(man3_syntheticexport_DATA): $(DOX_TAG)
$(man3_shmem_DATA): $(DOX_TAG)
$(man3_bitmap_DATA): $(DOX_TAG)
$(man3_helper_find_inside_DATA): $(DOX_TAG)
$(man3_helper_find_covering_DATA): $(DOX_TAG)
$(man3_helper_ancestors_DATA): $(DOX_TAG)
$(man3_helper_find_types_DATA): $(DOX_TAG)
$(man3_helper_find_cache_DATA): $(DOX_TAG)
$(man3_helper_find_misc_DATA): $(DOX_TAG)
$(man3_helper_distribute_DATA): $(DOX_TAG)
$(man3_helper_topology_sets_DATA): $(DOX_TAG)
$(man3_helper_nodeset_convert_DATA): $(DOX_TAG)
$(man3_helper_distances_DATA): $(DOX_TAG)
$(man3_helper_advanced_io_DATA): $(DOX_TAG)
$(man3_diff_DATA): $(DOX_TAG)
$(man3_cuda_DATA): $(DOX_TAG)
$(man3_glibc_sched_DATA): $(DOX_TAG)
$(man3_linux_DATA): $(DOX_TAG)
$(man3_linux_libnuma_DATA): $(DOX_TAG)
$(man3_openfabrics_DATA): $(DOX_TAG)

$(DOX_LETTERPDF): $(DOX_TAG)
$(DOX_HTML_DIR): $(DOX_TAG)
$(DOX_LATEX_DIR): $(DOX_TAG)
$(DOX_MAN_DIR): $(DOX_TAG)

# Additionally, we have to ensure that the A4 and Letter PDFs aren't
# built at the same time during a parallel build (e.g., "make -j X").
# So arbitrarily make the A4 PDF dependent upon the Letter PDF.
$(DOX_A4PDF): $(DOX_TAG) $(DOX_LETTERPDF)

endif HWLOC_BUILD_DOXYGEN



#
# Only remove the actual generated documentation files for maintainers
#

MAINTAINERCLEANFILES = $(DOX_TAG) \
    -r \
    $(DOX_HTML_DIR) \
    $(DOX_MAN_DIR) \
    $(DOX_LATEX_DIR) \
    $(DOX_A4PDF) \
    $(DOX_LETTERPDF) \
    $(DOX_HTML_README)

if HWLOC_BUILD_README

#
# Rules for creating the top-level README file.  There does not appear
# to be an easy way to know if AC_PATH_PROG found something in
# configure.ac (!), so put a run-time check here to see if we have
# w3c.
#
# Just like BUILD_DOXYGEN, BUILD_README will automatically be false if
# we're not building standalone.
#
readme: $(HWLOC_top_srcdir)/README
$(HWLOC_top_srcdir)/README: $(DOX_TAG)
	sed -n -f $(srcdir)/README.sed $(DOX_HTML_DIR)/index.html > $(DOX_HTML_README)
	LC_ALL=C $(HWLOC_W3_GENERATOR) $(DOX_HTML_README) \
	 | $(SED) -n -e 's/^   //' -e '/^Introduction$$/,$$p' \
	 | $(SED) -e '/^--*-$$/,$$ d' \
	 > $@
	echo >> $@
	echo >> $@
	echo "See https://www.open-mpi.org/projects/hwloc/doc/ for more hwloc documentation." >> $@
	rm -f $(DOX_HTML_README)

else

# When we don't have what's needed to build the README, nothing to do
readme: doc

endif

all-local: doc readme

SUBDIRS = examples
