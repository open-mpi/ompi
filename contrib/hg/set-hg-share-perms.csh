#!/bin/csh -f

if (! -d .hg) then
    echo "Not in top-level HG repository dir"
    exit 1
endif

echo Setting group to openmpi...
chgrp -R openmpi .

echo Setting all files to be group read/writable....
chmod -R g+rw .

echo Setting group "rwsx" perms on directories...
find . -type d -exec chmod g=rwsx {} \;

echo done
exit 0
