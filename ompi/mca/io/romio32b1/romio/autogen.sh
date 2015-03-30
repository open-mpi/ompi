#!/bin/sh

${AUTORECONF:-autoreconf} ${autoreconf_args:-"-vif"} -I confdb
