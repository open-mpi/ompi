import re
import textwrap
import consts
from consts import Lang

import pympistandard as std

categories = {}

def comment(message, lang=Lang.C, indentation=0):
    """
    Take in a message and return a commented version of it.
    """
    comment_lines = []
    width = 80
    #words = message.split()
    prefix = ""
    spaces = (indentation + 1) * ' '
    if lang == Lang.C or lang == Lang.CPP:
        comment_lines.append(spaces = "/*")
        prefix = " *"
    elif lang == Lang.FORTRAN:
        prefix ="!"

    prefix = spaces + prefix + ' '
    comment_lines.append(textwrap.wrap(message,
                                       width=width,
                                       initial_indent=prefix,
                                       subsequent_indent=prefix))
    # for word in words:
    #     line = [prefix]
    #     cols_left = width - len(prefix)
    #     len(word) <= cols_left:
    #         line.append(word)
    #     comment_lines.append()

    if lang == Lang.C or lang == Lang.CPP:
        comment_lines.append(spaces + "*/")
    return '\n'.join(comment_lines)

for category in consts.categories.values():
    name = category["name"]
    categories[name] = []
    for value in consts.consts.values():
        if value["category"] == name:
            categories[name].append(value)

def output_constant(const, use_enum):
    name = const["name"]
    abi_value = const["abi_value"]
    c_type = const["handle_types"]["c"]["type"]
    if c_type is None:
        return None
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
            line = output_constant(constant, use_enum)
            if line is not None:
                output.append(line)
        if use_enum:
            output.append("};\n")
    else:
        output.append(line)

# ============================= Function Prototypes ============================
std.use_api_version()

output.append("\n")
output.append("/* MPI API */\n")
for proc in std.all_iso_c_procedures():
    output.append(f"{proc.express.iso_c}\n")

output.append("\n")
output.append("/* Profiling MPI API */\n")
for proc in std.all_iso_c_procedures():
     output.append(f"{proc.express.profile.iso_c}\n")

# ================================ Final Output ================================
with open(consts.DIR / "abi.h", 'tw') as header_out:
    header_out.writelines(output)
