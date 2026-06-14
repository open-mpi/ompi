#!/usr/bin/env python3
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Convert Open MPI's Markdown CODE_OF_CONDUCT.md to reStructuredText."""

import argparse
import re
from pathlib import Path

HEADING_RE = re.compile(r"^(#{1,6})\s+(.+?)\s*#*\s*$")
REF_DEF_RE = re.compile(r"^\[([^\]]+)\]:\s*(\S+)\s*$")
REF_LINK_RE = re.compile(r"\[([^\]]+)\]\[([^\]]+)\]")
INLINE_LINK_RE = re.compile(r"\[([^\]]+)\]\(([^)]+)\)")
BULLET_RE = re.compile(r"^\s*[*+-]\s+")
UNDERLINES = ["=", "-", "~", "^", '"', "'"]


def _rst_link(text, url):
    return "`{} <{}>`_".format(text, url)


def _convert_links(line, refs):
    def repl_ref(match):
        text = match.group(1)
        ref = match.group(2)
        return _rst_link(text, refs.get(ref, ref))

    def repl_inline(match):
        return _rst_link(match.group(1), match.group(2))

    line = REF_LINK_RE.sub(repl_ref, line)
    line = INLINE_LINK_RE.sub(repl_inline, line)
    return line


def convert(lines):
    refs = {}
    for line in lines:
        match = REF_DEF_RE.match(line.strip())
        if match:
            refs[match.group(1)] = match.group(2)

    out = [
        "..",
        "   This file was generated from CODE_OF_CONDUCT.md by docs/Makefile.am.",
        "   Do not edit directly.",
        "",
    ]

    skip_next_blank = False
    prev_bullet = False
    for line in lines:
        if REF_DEF_RE.match(line.strip()):
            continue
        if skip_next_blank and not line.strip():
            skip_next_blank = False
            continue
        skip_next_blank = False

        match = HEADING_RE.match(line)
        if match:
            if out and out[-1] != "":
                out.append("")
            title = _convert_links(match.group(2), refs)
            level = min(len(match.group(1)) - 1, len(UNDERLINES) - 1)
            out.extend([title, UNDERLINES[level] * len(title), ""])
            skip_next_blank = True
            prev_bullet = False
        else:
            is_bullet = bool(BULLET_RE.match(line))
            if is_bullet and out and out[-1] != "" and not prev_bullet:
                out.append("")
            out.append(_convert_links(line, refs))
            prev_bullet = is_bullet

    while out and out[-1] == "":
        out.pop()
    return "\n".join(out) + "\n"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    input_path = Path(args.input)
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(
        convert(input_path.read_text(encoding="utf-8").splitlines()),
        encoding="utf-8")


if __name__ == "__main__":
    main()
