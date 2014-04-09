#!/bin/sh

cd /l/osl/www/www.open-mpi.org

# Absolutely ensure that we have a umask of 2 so that others can
# run "svn up" in this tree, too
umask 2

svn up

date >> /tmp/bogus-mpiteam
