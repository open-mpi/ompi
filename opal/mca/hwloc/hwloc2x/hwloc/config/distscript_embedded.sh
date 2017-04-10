#!/bin/sh

chmod u+w $2
makefiles="$2/doc/Makefile.am $2/doc/examples/Makefile.am $2/doc/doxygen-config.cfg.in $2/utils/Makefile.am $2/utils/hwloc/Makefile.am $2/utils/lstopo/Makefile.am $2/utils/netloc/infiniband/Makefile.am $2/utils/netloc/draw/Makefile.am $2/utils/netloc/mpi/Makefile.am $2/tests/Makefile.am $2/tests/hwloc/Makefile.am $2/tests/hwloc/linux/Makefile.am $2/tests/hwloc/linux/allowed/Makefile.am $2/tests/hwloc/linux/gather/Makefile.am $2/tests/hwloc/x86/Makefile.am $2/tests/hwloc/xml/Makefile.am $2/tests/hwloc/ports/Makefile.am $2/tests/hwloc/rename/Makefile.am $2/tests/hwloc/linux/allowed/test-topology.sh.in $2/tests/hwloc/linux/gather/test-gather-topology.sh.in $2/tests/hwloc/linux/test-topology.sh.in $2/tests/hwloc/x86/test-topology.sh.in $2/tests/hwloc/xml/test-topology.sh.in $2/tests/hwloc/wrapper.sh.in $2/utils/hwloc/hwloc-compress-dir.in $2/utils/hwloc/hwloc-gather-topology.in $2/utils/hwloc/test-hwloc-annotate.sh.in $2/utils/hwloc/test-hwloc-calc.sh.in $2/utils/hwloc/test-hwloc-compress-dir.sh.in $2/utils/hwloc/test-hwloc-diffpatch.sh.in $2/utils/hwloc/test-hwloc-distrib.sh.in $2/utils/hwloc/test-hwloc-info.sh.in $2/utils/hwloc/test-fake-plugin.sh.in $2/utils/hwloc/test-hwloc-dump-hwdata/Makefile.am $2/utils/hwloc/test-hwloc-dump-hwdata/test-hwloc-dump-hwdata.sh.in $2/utils/lstopo/test-lstopo.sh.in $2/contrib/systemd/Makefile.am $2/contrib/misc/Makefile.am $2/tests/netloc/Makefile.am $2/tests/netloc/tests.sh.in $2/utils/lstopo/lstopo-windows.c"
rm -f $makefiles
for i in $makefiles; do
    [ -d $(dirname $i) ] || mkdir -p $(dirname $i)
    cat > $i << EOF
# This is a dummy file that is not needed in embedded mode,
# but sadly, automake *requires* it
EOF
done

