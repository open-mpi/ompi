#!/usr/bin/env perl
#
# Copyright (c) 2013-2018 Intel, Inc. All rights reserved
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

use strict;
use warnings;
use mtl_ofi_send_opt;
use mtl_ofi_isend_opt;
use mtl_ofi_irecv_opt;
use mtl_ofi_iprobe_opt;
use mtl_ofi_improbe_opt;
use opt_common::mtl_ofi_opt_common;

my $MTL_OFI_HEADER =
'/*
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mtl_ofi.h"';

my $specialization_file = $ARGV[0];
my $specialization_type = $specialization_file;
$specialization_type =~ s{\.[^.]+$}{};
my $sym_table_type = $specialization_type;
$sym_table_type =~ s/_opt//g;

open my $gen_file, ">", $specialization_file;

#
# Generate the Specialized functions & symbol table for the specified file.
#
print $gen_file "$MTL_OFI_HEADER\n\n";

my $GEN_FUNC = $specialization_type . "::gen_funcs\(\$gen_file, \"FUNC\"\)";
my $GEN_SYM = $specialization_type . "::gen_funcs\(\$gen_file, \"SYM\"\)";
my $SYM_TABLE = "ompi_" . $sym_table_type . "_symtable";

eval $GEN_FUNC;

my $SYM_FUNC_HEADER = opt_common::mtl_ofi_opt_common::gen_sym_function_header($SYM_TABLE);
print $gen_file "$SYM_FUNC_HEADER\n";

eval $GEN_SYM;

my $SYM_FUNC_FOOTER = opt_common::mtl_ofi_opt_common::gen_sym_function_footer();
print $gen_file "$SYM_FUNC_FOOTER\n\n";
close($gen_file);
exit(0);
###
