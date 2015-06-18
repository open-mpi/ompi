#!/bin/sh

# Run all the rest of the Autotools
echo "==> Running autoreconf";
autoreconf ${autoreconf_args:-"-ivf"}
