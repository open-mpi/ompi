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
package opt_common::mtl_ofi_opt_common;

#
# Generate the extension for functions and symbols based off the flags.
#
sub gen_flags_ext {
    my $OP_FLAGS = "";
    my @name_flags = @{$_[0]};
    my $num_flags = $#name_flags;
    for my $flag (@name_flags) {
        $OP_FLAGS = $OP_FLAGS . $flag;
        if ($num_flags--) {
            $OP_FLAGS = $OP_FLAGS . '_';
        }
    }
    return $OP_FLAGS;
}

#
# Generate the header for the specialized symbol table init function.
#
sub gen_sym_function_header {
    my $MTL_OFI_SYM_TYPE = $_[0];
    my $header =
"void ".$MTL_OFI_SYM_TYPE."_init(struct ompi_mtl_ofi_symtable *sym_table)
{";
    return $header;
}
###

#
# Generate the footer for the specialized symbol table init function.
#
sub gen_sym_function_footer {
    my $footer =
"}";
    return $footer;
}
###

1;
