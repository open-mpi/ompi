# Copyright © 2009      INRIA, Université Bordeaux 1
# Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.

AM_CPPFLAGS = $(HWLOC_CPPFLAGS)

DOCDIR = $(HWLOC_top_builddir)/doc
DOX_CONFIG = $(HWLOC_top_srcdir)/doc/doxygen.cfg

DOX_DIR = doxygen-doc
DOX_HTML_DIR = $(DOX_DIR)/html
DOX_MAN_DIR = $(DOX_DIR)/man
DOX_LATEX_DIR = $(DOX_DIR)/latex
DOX_A4PDF = doxygen-doc/$(PACKAGE)-a4.pdf
DOX_LETTERPDF = doxygen-doc/$(PACKAGE)-letter.pdf
DOX_TAG = $(DOX_DIR)/$(PACKAGE).tag

#
# The goal is that Doxygen output (i.e., the documentation) is
# included in release tarballs; there is no need to build anything
# from tarballs.  Developers will need to build the docs the first
# time they build after a checkout (or if something in the docs
# changes, etc.).
#
# Note that by listing directories in EXTRA_DIST, we pick up the whole
# tree (e.g., everything in the man and latex directories).
#

PREBUILT_IMAGES = images/dudley.png images/emmett.png images/hagrid.png
image_built_sources = images/diagram.png images/diagram.eps

EXTRA_DIST = \
        hwloc.doxy \
        doxygen.css \
        doxygen.cfg \
        images/diagram.fig \
        www.open-mpi.org.cfg \
        www.open-mpi.org-css.inc \
        www.open-mpi.org-footer.inc \
        www.open-mpi.org-header.inc \
        $(PREBUILT_IMAGES) \
        $(image_built_sources)

#
# Files where the doxygen inputs live (i.e., dependencies).  Make the
# generated tagfile depend on these files, which will force them to be
# regenerated (i.e., re-run doxygen) whenever any of these files
# change.
#

dox_inputs = $(DOX_CONFIG) \
       $(srcdir)/hwloc.doxy \
       $(HWLOC_top_srcdir)/include/hwloc.h \
       $(HWLOC_top_srcdir)/include/hwloc/helper.h \
       $(HWLOC_top_srcdir)/include/hwloc/cpuset.h \
       $(HWLOC_top_srcdir)/include/hwloc/glibc-sched.h \
       $(HWLOC_top_srcdir)/include/hwloc/linux.h \
       $(HWLOC_top_srcdir)/include/hwloc/linux-libnuma.h \
       $(HWLOC_top_srcdir)/include/hwloc/openfabrics-verbs.h

#
# Create the images that we need for the PDF output and the HTML
# output.  There is not an easy way to check if the output of
# AC_PATH_PROG found something in configure.ac (!), so we have to put
# a run-time check here to see if fig2dev was found.  :-(
#
# Note that BUILD_DOXYGEN will automatically be false if we're not
# building standalone.
#

if HWLOC_BUILD_DOXYGEN
BUILT_SOURCES = $(image_built_sources)

images/diagram.png: $(srcdir)/images/diagram.fig
	$(MKDIR_P) images
	@if test "x$(FIG2DEV)" = "x"; then \
	    echo "ERROR: Cannot find the 'fig2dev' executable -- cannot make $@"; \
	    exit 1; \
	fi
	$(FIG2DEV) -L png $< $@

images/diagram.eps: $(srcdir)/images/diagram.fig
	$(MKDIR_P) images
	@if test "x$(FIG2DEV)" = "x"; then \
	    echo "ERROR: Cannot find the 'fig2dev' executable -- cannot make $@"; \
	    exit 1; \
	fi
	$(FIG2DEV) -L eps $< $@
endif

#
# Rules for running doxygen.  It depends on the built images and the
# dox_inputs.
# Remove useless manpages, they have too long filenames anyway because
# of nested structurre/union declarations.
#

if HWLOC_BUILD_DOXYGEN
$(DOX_TAG): $(BUILT_SOURCES) $(dox_inputs) $(PREBUILT_IMAGES)
	rm -fr $(DOX_DIR)
	$(DOXYGEN) $(DOX_CONFIG)
	-sed -i \
		-e 's/__hwloc_restrict/restrict/g' \
		-e 's/\\_\\-\\_\\-hwloc\\_\\-restrict/restrict/g' \
		-e 's/__hwloc_attribute_unused//g' \
		-e 's/\\_\\-\\_\\-hwloc\\_\\-attribute\\_\\-unused//g' \
		-e 's/__hwloc_attribute_malloc//g' \
		-e 's/\\_\\-\\_\\-hwloc\\_\\-attribute\\_\\-malloc//g' \
		-e 's/__hwloc_attribute_const//g' \
		-e 's/\\_\\-\\_\\-hwloc\\_\\-attribute\\_\\-const//g' \
		-e 's/__hwloc_attribute_pure//g' \
		-e 's/\\_\\-\\_\\-hwloc\\_\\-attribute\\_\\-pure//g' \
		-e 's/__hwloc_attribute_deprecated//g' \
		-e 's/\\_\\-\\_\\-hwloc\\_\\-attribute\\_\\-deprecated//g' \
		-e 's/HWLOC_DECLSPEC//g' \
		-e 's/HWLOC\\_\\-DECLSPEC//g' \
		-e 's/__inline/inline/g' \
		-e 's/\\_\\-\\_\\-inline/inline/g' \
		$(DOX_DIR)/html/*.html $(DOX_DIR)/latex/*.tex $(DOX_DIR)/man/man3/*.3
	@echo "Work-around spurious leading _ in doxygen filenames..."
	(cd $(DOX_DIR)/man/man3 ; \
	for i in _hwloc* ; do \
		[ ! -f $$i ] || mv $$i $${i#_} ; \
	done)
	@echo "Removing useless manpages..."
	@mkdir $(DOX_DIR)/man.tmp
	@mv $(man3_MANS) $(DOX_DIR)/man.tmp/
	@rm -rf $(DOX_DIR)/man/man3
	@mv $(DOX_DIR)/man.tmp $(DOX_DIR)/man/man3
if HWLOC_DOXYGEN_BROKEN_SHORT_NAMES
	@echo "Work-around buggy doxygen filenames..."
	-@mv -f $(DOX_DIR)/html/termsanddefs.html $(DOX_DIR)/html/a00001.html
	-@mv -f $(DOX_DIR)/latex/termsanddefs.tex $(DOX_DIR)/latex/a00001.tex
	-@mv -f $(DOX_DIR)/html/tools.html $(DOX_DIR)/html/a00002.html
	-@mv -f $(DOX_DIR)/latex/tools.tex $(DOX_DIR)/latex/a00002.tex
	-@mv -f $(DOX_DIR)/html/envvar.html $(DOX_DIR)/html/a00003.html
	-@mv -f $(DOX_DIR)/latex/envvar.tex $(DOX_DIR)/latex/a00003.tex
	-@mv -f $(DOX_DIR)/html/threadsafety.html $(DOX_DIR)/html/a00004.html
	-@mv -f $(DOX_DIR)/latex/threadsafety.tex $(DOX_DIR)/latex/a00004.tex
	-@mv -f $(DOX_DIR)/html/embed.html $(DOX_DIR)/html/a00005.html
	-@mv -f $(DOX_DIR)/latex/embed.tex $(DOX_DIR)/latex/a00005.tex
	-@mv -f $(DOX_DIR)/html/switchfromplpa.html $(DOX_DIR)/html/a00006.html
	-@mv -f $(DOX_DIR)/latex/switchfromplpa.tex $(DOX_DIR)/latex/a00006.tex
endif
endif

#
# Rules for building the PDF
#

if HWLOC_BUILD_DOXYGEN

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
	sed -e 's/a4paper/letterpaper/g' -e 's/\\usepackage{a4wide}//' refman.tex > letter-refman.tex; \
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
endif

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

if HWLOC_BUILD_DOXYGEN
all-local: $(DOX_TAG)
	if test -d $(DOX_HTML_DIR) -a -f $(DOX_HTML_DIR)/doxygen.css; then \
	    cp -f $(srcdir)/doxygen.css $(DOX_HTML_DIR); \
	fi
endif

#
# Un/install the generated PDF and man pages (just like BUILD_DOXYGEN,
# INSTALL_DOXYGEN will automatically be false if we're not building in
# standalone mode).
#

if HWLOC_INSTALL_DOXYGEN
dist_pdf_DATA = $(DOX_A4PDF) $(DOX_LETTERPDF)
endif

#
# Install the HWLOC_* and hwloc_* man pages.  It would be great to
# figure out how to not have to list every installable man page here
# in the Makefile.am...  :-(
#

if HWLOC_INSTALL_DOXYGEN
man3_MANS = \
        $(DOX_MAN_DIR)/man3/HWLOC_CPUBIND_PROCESS.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_CPUBIND_STRICT.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_CPUBIND_THREAD.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_CACHE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_CORE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_MACHINE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_MISC.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_GROUP.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_NODE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_PU.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_SOCKET.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_OBJ_SYSTEM.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_DEPTH_MULTIPLE.3 \
        $(DOX_MAN_DIR)/man3/HWLOC_TYPE_DEPTH_UNKNOWN.3 \
        $(DOX_MAN_DIR)/man3/hwloc_compare_types.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpubind_policy_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_alloc.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_free.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_dup.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_copy.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_all_but_cpu.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_and.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_andnot.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_clr.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_clr_range.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_compare.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_compare_first.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_fill.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_first.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_foreach_begin.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_foreach_end.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_glibc_sched_affinity.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_ith_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_linux_libnuma_nodemask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_linux_libnuma_ulongs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_string.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_from_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_intersects.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_isequal.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_isfull.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_isincluded.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_isset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_iszero.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_last.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_next.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_not.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_or.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_set.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_set_range.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_singlify.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_asprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_glibc_sched_affinity.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_ith_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_linux_libnuma_nodemask.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_linux_libnuma_ulongs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_to_ulong.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_weight.3 \
        $(DOX_MAN_DIR)/man3/hwloc_cpuset_xor.3 \
        $(DOX_MAN_DIR)/man3/hwloc_distribute.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_closest_objs.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_common_ancestor_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_cache_covering_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_child_covering_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_covering_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_depth_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_allowed_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_complete_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_online_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_topology_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_first_largest_obj_inside_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_largest_objs_inside_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_inside_cpuset_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_inside_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_child.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_covering_cpuset_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_covering_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_inside_cpuset_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_inside_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_next_obj_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_pu_obj_by_os_index.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_below_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_below_array_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_inside_cpuset_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_inside_cpuset_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_obj_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_ancestor_obj_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_ancestor_obj_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_shared_cache_covering_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_root_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_type_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_by_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_nbobjs_by_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_type_or_above_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_type_or_below_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_ibv_get_device_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwloc_linux_parse_cpumap_file.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_cache_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_machine_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_u_hwloc_group_attr_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_memory_s.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_cpuset_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_is_in_subtree.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_attr_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_snprintf.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_of_string.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_string.3 \
        $(DOX_MAN_DIR)/man3/hwloc_obj_type_t.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_proc_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_get_thread_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_proc_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_set_thread_cpubind.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_check.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_destroy.3 \
	$(DOX_MAN_DIR)/man3/hwloc_topology_export_xml.3 \
	$(DOX_MAN_DIR)/man3/hwloc_topology_insert_misc_object_by_cpuset.3 \
	$(DOX_MAN_DIR)/man3/hwloc_topology_insert_misc_object_by_parent.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_flags_e.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_depth.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_get_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_ignore_all_keep_structure.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_ignore_type.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_ignore_type_keep_structure.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_init.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_is_thissystem.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_load.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_flags.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_fsroot.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_pid.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_synthetic.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_set_xml.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_discovery_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_cpubind_support.3 \
        $(DOX_MAN_DIR)/man3/hwloc_topology_t.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_binding.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_configuration.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_conversion.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_cpuset.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_creation.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_glibc_sched.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_binding.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_cache.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_covering.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_coverings.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_find_inside.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_traversal.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_traversal_basic.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_helper_types.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_information.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_linux_libnuma_bitmask.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_linux_libnuma_nodemask.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_linux_libnuma_ulongs.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_objects.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_traversal.3 \
        $(DOX_MAN_DIR)/man3/hwlocality_types.3
endif

#
# Put in these rules to force the generation of the man pages and get
# all the dependencies right.  Additionally, this forces a
# serialization during parallel builds (e.g., "make -j X") to ensure
# that we don't get partial target errors.
#

if HWLOC_BUILD_DOXYGEN
$(man3_MANS): $(DOX_TAG)
$(DOX_A4PDF): $(DOX_TAG)
$(DOX_LETTERPDF): $(DOX_TAG)
$(DOX_HTML_DIR): $(DOX_TAG)
$(DOX_LATEX_DIR): $(DOX_TAG)
$(DOX_MAN_DIR): $(DOX_TAG)
endif

#
# Make sure that the documentation example works
#

if HWLOC_BUILD_TESTS
TESTS = hwloc-hello
check_PROGRAMS = $(TESTS)
endif HWLOC_BUILD_TESTS

hwloc_hello_LDADD = $(HWLOC_top_builddir)/src/libhwloc.la
hwloc_hello_DEPENDENCIES = $(hwloc_hello_LDADD)

#
# Only remove the actual generated documentation files for maintainers
#

MAINTAINERCLEANFILES = $(DOX_TAG) \
    -r \
    $(DOX_HTML_DIR) \
    $(DOX_MAN_DIR) \
    $(DOX_LATEX_DIR) \
    $(DOX_A4PDF) \
    $(DOX_LETTERPDF)

#
# Rules for creating the top-level README file.  There does not appear
# to be an easy way to know if AC_PATH_PROG found something in
# configure.ac (!), so put a run-time check here to see if we have
# w3c.  
#
# Just like BUILD_DOXYGEN, BUILD_README will automatically be false if
# we're not building standalone.
#

if HWLOC_BUILD_README
doc readme: all $(HWLOC_top_srcdir)/README
$(HWLOC_top_srcdir)/README: $(DOX_HTML_DIR)
	LC_ALL=C $(HWLOC_W3_GENERATOR) $(DOX_HTML_DIR)/index.html | sed -n -e 's/^   //' -e '/^Introduction$$/,$$p' > $@
else
doc readme: no-build-readme
$(HWLOC_top_srcdir)/README: no-build-readme

no-build-readme:
	@echo "ERROR: You do not have the 'w3m' or 'lynx' executables to build the README, or do not have the 'doxygen' executable to build the docs."
	@echo "ERROR: Cannot continue."
	@exit 1
endif

#DOCUPLOADHOST=sync.bordeaux.inria.fr
#DOCUPLOADOPTS=
#doc-upload: @DX_DOCDIR@/html
#	echo FIXME
#	false
#	rsync -avz --delete --chmod=ug+rwX,o+rX $(DOCUPLOADOPTS) @DX_DOCDIR@/html/ $(DOCUPLOADHOST):/web/runtime/libtopology/doc/
