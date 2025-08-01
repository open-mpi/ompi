# Copyright (c) 2025      Joe Downs. All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

import argparse
import re
import json
from pathlib import Path

import pympistandard as std

# ============================= Constants / Globals ============================
DIR = Path(".")
OUTPUT = DIR / "mpi.h"
INPUT = DIR / "abi.h.in"
JSON_PATH = DIR / "abi.json"

ABI_INTERNAL = "_ABI_INTERNAL"

categories_dict = {}
MANGLE_NAMES = False

INTERNAL_DATATYPES = [
    "MPI_Comm",
    "MPI_Datatype",
    "MPI_Errhandler",
    "MPI_File",
    "MPI_Group",
    "MPI_Info",
    "MPI_Message",
    "MPI_Op",
    "MPI_Request",
    "MPI_Session",
    "MPI_Status",
    "MPI_Win",
    "MPI_Comm_copy_attr_function",
    "MPI_Comm_delete_attr_function",
    # TODO: these two are deprecated, get rid of them
    "MPI_Copy_function",
    "MPI_Delete_function",
]

ENUM_CATEGORIES = [
    "ERROR_CLASSES",
    "MODE_CONSTANTS",
    "ASSORTED_CONSTANTS",
    "THREADS_CONSTANTS",
    "FILE_OPERATIONS_CONSTANTS",
    "DATATYPE_DECODING_CONSTANTS",
    "F90_DATATYPE_MATCHING_CONSTANTS",
    "COMMUNICATOR_GROUP_COMP_RESULTS",
    "TOPOLOGIES",
    "COMMUNICATOR_SPLIT_TYPE",
    "WINDOW_LOCK_TYPE_CONSTANTS",
    "WINDOW_CREATE_FLAVORS",
    "WINDOW_MODELS",
    "FILE_POS_CONSTANTS",
    "FILE_OP_CONSTANTS",
    "ENV_INQ_AND_ATTR_KEYS",
    "FORTRAN_STATUS_ARRAY_SIZE_AND_INDEX_C",
    "C_PREPROCESSOR_CONSTANTS_FORTRAN_PARAMETERS",
    "TOOL_INFO_IFACE_VERBOSITY_LEVELS",
    "TOOL_INFO_IFACE_VAR_ASSOCIATIONS",
    "TOOL_INFO_IFACE_VAR_SCOPES",
    "TOOL_INFO_IFACE_PVAR_CLASSES",
    "TOOL_INFO_IFACE_SOURCE_ORDERINGS",
    "TOOL_INFO_IFACE_CB_SAFETY_REQ_LEVELS",
]


# ============================== Argument Parsing ==============================
parser = argparse.ArgumentParser()
parser.add_argument("--abi-json", type=str, help=f"path to ABI JSON file [{DIR}/]")
parser.add_argument("-i", "--input", type=str, help="input path for the .h.in file")
parser.add_argument("-o", "--output", type=str, help="output path for the header file")
parser.add_argument("--mangle-names", help="enable name mangling for constants and datatypes", action="store_true")
parser.add_argument("--no-mangle", help="disable name mangling (default)", action="store_true")

args = parser.parse_args()

if args.abi_json:
    JSON_PATH = Path(args.abi_json)
if args.mangle_names:
    MANGLE_NAMES = True
if args.no_mangle:
    MANGLE_NAMES = False
if args.output:
    OUTPUT = Path(args.output)
if args.input:
    INPUT = Path(args.input)

# ================================== Load JSON =================================
with open(JSON_PATH) as f:
    abi = json.load(f)

CONSTS = abi["constants"]
CATEGORIES = abi["categories"]

# ==============================================================================

# Populating the `categories` dictionary
for category in CATEGORIES.values():
    name = category["name"]
    categories_dict[name] = []
    for value in CONSTS.values():
        if value["category"] == name:
            categories_dict[name].append(value)

# ================================== Functions =================================
# TODO: we need to add/fix/figure out the pympistandard's way for properly
# defining callback functions
def cb_declaration(proc_expression):
    func_str = str(proc_expression).replace(r"\ldots", "...")
    func_str_list = func_str.split()
    func_name, arg_1 = func_str_list[2].split("(")
    decl_string = f"{' '.join(func_str_list[:2])} ({func_name})({arg_1} {' '.join(func_str_list[3:])};\n"
    return decl_string

def output_constant(const, use_enum: bool, mangle_name: bool):
    name = const["name"]
    abi_value = const["abi_value"]
    c_type = const["handle_types"]["c"]["type"]
    if c_type is None:
        return None
    if mangle_name:
        name = f"{name}{ABI_INTERNAL}"
        c_type = f"{c_type}{ABI_INTERNAL}"
    def_name = f"#define {name}"
    if use_enum:
        def_name = f"    {name}"
        value = f"= {abi_value},"
    elif c_type == "int":
        value = f"{abi_value}"
    else:
        value = f"(({c_type}) {abi_value})"
    return def_name + " " * (45 - len(def_name)) + value + "\n"

# ========================= Manipulate Template Header =========================
lines = []
with open(INPUT, 'r') as header_in:
    lines = header_in.readlines()

# Match lines that start with `$CATEGORY:`. Any amount of whitespace is allowed
# in the case of indenting. However, only preceding whitespace is allowed so
# that commented-out lines are NOT included.
category_pattern = re.compile(r"^[\s]*\$CATEGORY:([A-Z_0-9]+)\$$")
output = []

for line in lines:
    category = category_pattern.search(line)
    # If there's no match, category is None. Then we just want to preserve the
    # line as-is.
    if category:
        category = category.group(1)
        use_enum = False
        # Only some values should be in `enums`, otherwise just use `#define`s
        if category in ENUM_CATEGORIES:
            use_enum = True
        if use_enum:
            output.append("enum {\n")
        # Print out each `#define` / assignment for the constants
        for constant in categories_dict[category]:
            line = output_constant(constant, use_enum, MANGLE_NAMES)
            if line is not None:
                output.append(line)
        if use_enum:
            output.append("};\n")
    else:
        output.append(line)

# ============================= Function Prototypes ============================
std.use_api_version()

output.append("\n")
output.append("/* Callback functions */\n")
for proc in std.CALLBACKS.values():
    output.append(cb_declaration(proc.express.iso_c))
    if proc.has_embiggenment():
        output.append(cb_declaration(proc.express.embiggen.iso_c))

output.append("\n")
output.append("/* MPI API */\n")
for proc in std.all_iso_c_procedures():
    output.append(f"{proc.express.iso_c};\n")
    if proc.has_embiggenment():
        output.append(f"{proc.express.embiggen.iso_c};\n")

output.append("\n")
output.append("/* Profiling MPI API */\n")
for proc in std.all_iso_c_procedures():
     output.append(f"{proc.express.profile.iso_c};\n")
     # TODO: this is a hack and we need to add/fix/figure out the pympistandard
     # module to natively print out an embiggened version
     if proc.has_embiggenment():
         binding = proc.express.embiggen.iso_c.__str__().split()
         output.append(f"{binding[0]} P{' '.join(binding[1:])};\n")

# ================================ Final Output ================================
output.append("""#ifndef _ABI_INTERNAL_
#define _ABI_INTERNAL_
#include "stddef.h"
#include "stdint.h"
""")
output.append("#endif /* _ABI_INTERNAL_ */")

# Iterate through all lines and replace datatypes with their internal ABI
# counterparts
if MANGLE_NAMES:
    for i, line in enumerate(output):
        mangled_line = line
        for datatype in INTERNAL_DATATYPES:
            # Need to include the extra space here or else we'll edit functions
            # like "MPI_Group_difference"
            datatype_pattern = r"([\( ]?)(" + datatype + r")([; \*]{1})"
            mangled_line = re.sub(datatype_pattern, f"\\g<1>\\g<2>{ABI_INTERNAL}\\g<3>", mangled_line)
        output[i] = mangled_line

with open(OUTPUT, 'tw') as header_out:
    header_out.writelines(output)
