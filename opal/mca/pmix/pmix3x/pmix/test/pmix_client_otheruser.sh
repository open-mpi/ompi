#!/bin/bash

OTHERUSER=dummy

# To test sec:native we run client as a different user e.g.
#
#  ./pmix_test -e ./pmix_client_otheruser.sh
#
# Prerequisites:
# - ensure directories leading up to source tree are o+x
# - add dummy user and set OTHERUSER to its name
# - give yourself passwordless sudo capability to that user
#
# The test should fail with message similar to
#   PMIX ERROR: INVALID-CREDENTIAL in file
#       ../src/server/pmix_server_listener.c at line 524
#

sudo --user $OTHERUSER --preserve-env ./pmix_client $*
