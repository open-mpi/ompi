#
# Best attempt at a sed script to remove (almost) all combinations
# of C++ namespace occurences: While std. 3-line occurences with any
# combination of spaces and ordering of [c_plusplus,__cplusplus] can
# be detected, sed does not allow multi-line (here >3 lines) parsing.
# This all depends on the number of "N" the macro swallows.
#

# C_PLUSPLUS || __CPLUSPLUS
/#if *defined *(c_plusplus) *|| *defined *(__cplusplus)/{
N
N
s/\#if *defined *(c_plusplus) *|| *defined *(__cplusplus) *\n}\n#endif */END_C_DECLS/
s/\#if *defined *(c_plusplus) *|| *defined *(__cplusplus) *\n *extern *"C" *{ *\n\#endif */BEGIN_C_DECLS/
}


# __CPLUSPLUS || C_PLUSPLUS
/#if *defined *(__cplusplus) *|| *defined *(c_plusplus)/{
N
N
s/\#if *defined *(__cplusplus) *|| *defined *(c_plusplus) *\n}\n#endif */END_C_DECLS/
s/\#if *defined *(__cplusplus) *|| *defined *(c_plusplus) *\n *extern *"C" *{ *\n\#endif */BEGIN_C_DECLS/
}

# C_PLUSPLUS
/#if *defined *(c_plusplus)/{
N
N
s/\#if *defined *(c_plusplus) *\n}\n#endif */END_C_DECLS/
s/\#if *defined *(c_plusplus) *\n *extern *"C" *{ *\n\#endif *\n/BEGIN_C_DECLS/
}

# __CPLUSPLUS
/#if *defined *(__cplusplus)/{
N
N
s/\#if *defined *(__cplusplus) *\n}\n#endif */END_C_DECLS/
s/\#if *defined *(__cplusplus) *\n *extern *"C" *{ *\n\#endif */BEGIN_C_DECLS/
}

