import re
import textwrap
import consts
from consts import Lang

import pympistandard as std

# ============================= Constants / Globals ============================

ABI_INTERNAL = "_ABI_INTERNAL"

categories = {}
mangle_names = True

internal_datatypes = [
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
    "MPI_Win",
]

# Populating the `categories` dictionary
for category in consts.categories.values():
    name = category["name"]
    categories[name] = []
    for value in consts.consts.values():
        if value["category"] == name:
            categories[name].append(value)

# ================================== Functions =================================

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
with open(consts.DIR / "abi.h.in", 'r') as header_in:
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
        if category in consts.ENUM_CATEGORIES:
            use_enum = True
        if use_enum:
            output.append("enum {\n")
        # Print out each `#define` / assignment for the constants
        for constant in categories[category]:
            line = output_constant(constant, use_enum, mangle_names)
            if line is not None:
                output.append(line)
        if use_enum:
            output.append("};\n")
    else:
        output.append(line)

# ============================= Function Prototypes ============================
# TODO: we need to add/fix/figure out the pympistandard's way for properly
# defining callback functions
def cb_declaration(proc_expression):
    func_str = str(proc_expression).replace(r"\ldots", "...")
    func_str_list = func_str.split()
    func_name, arg_1 = func_str_list[2].split("(")
    decl_string = f"{' '.join(func_str_list[:2])} ({func_name})({arg_1} {' '.join(func_str_list[3:])};\n"
    return decl_string

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
if mangle_names:
    for i, line in enumerate(output):
        mangled_line = line
        for datatype in internal_datatypes:
            mangled_line = mangled_line.replace(datatype, f"{datatype}{ABI_INTERNAL}")
        output[i] = mangled_line

with open(consts.DIR / "abi.h", 'tw') as header_out:
    header_out.writelines(output)
