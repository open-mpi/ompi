# VampirTrace function filter specification
#
# Syntax: <functions> -- <limit> [S:<[min-]max-stack-level>] [R]
#     or: <groups>    -- <limit> [S:<[min-]max-stack-level>] [R] G
#
# functions, groups  Semicolon-separated list of functions/groups.
#                    (can contain wildcards)
#
# limit              call limit
#                    Stop recording of function/group when the specified call
#                    limit is reached.
#                    (0 = don't record function/group, -1 = record unlimited)
#
# S:<[min-]max-stack-level>
#                    minimum/maximum call stack level
#                    Don't record function/group called beyond the specified
#                    stack level boundaries.
#                    (values must be > 0, only valid if call limit is != 0)
#
# R                  Attribute for recursive filtering.
#                    Don't record callees of filtered function/group.
#
# G                  Attribute for filtering function groups.
#
# Example:
#
#   add;sub;mul;div -- 1000
#   MATH            -- 500 G
#   *               -- 3000000 S:5-10
#
# These filter directives cause that the functions "add", "sub", "mul", and
# "div" will be recorded at most 1000 times and all the functions of the group
# "MATH" at most 500 times. The remaining functions (*) will only be recorded
# when they are called between call stack level 5 and 10 but at most 3000000
# times.
#
# Rank Specific Filtering:
#
# Use the '@' clauses to restrict all the following filter directives to the
# given ranks. The rank selection must be given as a list of <from> - <to>
# pairs or single values.
# The optional argument "-- OFF" disables the given ranks completely, regardless
# of following filter rules.
#
# Example:
#
#   @ 35 - 42 -- OFF
#   @ 4 - 10, 20 - 29, 34
#   foo;bar -- 2000
#   * -- 0
#
# This example defines two limits for the ranks 4 - 10, 20 - 29, and 34. The
# first line disables the ranks 35 - 42 completely.
