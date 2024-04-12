#
# Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
# Copyright (c) 2023-2024 Nanook Consulting  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Install the prterun RST "source" files so that other projects can
# include them in their documentation, even if they do not have access
# to the PRTE source tree.
#
# Note that we install these files into the "data" directory (vs. the
# "doc" directory) so that packagers won't split these files into a
# "docs" sub-package that may not be installed when we build other
# packages that want to use these RST files.
rstdir = $(prtedatadir)/rst/prrte-rst-content
dist_rst_DATA = \
    cli-allow-run-as-root.rst \
    cli-bind-to.rst \
    cli-dash-host.rst \
    cli-debug-daemons-file.rst \
    cli-debug-daemons.rst \
    cli-display.rst \
    cli-dvm-hostfile.rst \
    cli-dvm.rst \
    cli-forward-signals.rst \
    cli-launcher-hostfile.rst \
    cli-leave-session-attached.rst \
    cli-map-by.rst \
    cli-noprefix.rst \
    cli-output.rst \
    cli-personality.rst \
    cli-pmixmca.rst \
    cli-prefix.rst \
    cli-prtemca.rst \
    cli-rank-by.rst \
    cli-runtime-options.rst \
    cli-stream-buffering.rst \
    cli-tune.rst \
    cli-x.rst \
    definitions-pes.rst \
    definitions-slots.rst \
    deprecated-bind-to-core.rst \
    deprecated-display-allocation.rst \
    deprecated-display-devel-allocation.rst \
    deprecated-display-devel-map.rst \
    deprecated-display-map.rst \
    deprecated-display-topo.rst \
    deprecated-gmca.rst \
    deprecated-mca.rst \
    deprecated-merge-stderr-to-stdout.rst \
    deprecated-output-directory.rst \
    deprecated-output-filename.rst \
    deprecated-report-bindings.rst \
    deprecated-tag-output.rst \
    deprecated-timestamp-output.rst \
    deprecated-xml.rst \
    detail-hostfiles.rst \
    detail-hosts-cli.rst \
    detail-hosts-relative-indexing.rst \
    detail-hosts-rm.rst \
    detail-placement.rst \
    detail-placement-examples.rst \
    detail-placement-rankfiles.rst \
    detail-placement-fundamentals.rst \
    detail-placement-deprecated.rst \
    detail-placement-diagnostics.rst \
    detail-placement-limits.rst
