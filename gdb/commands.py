# Save this as gdb_helpers.py
import gdb


class DumpNamespace(gdb.Command):
    """Dump all variables in a namespace"""

    def __init__(self):
        super(DumpNamespace, self).__init__("dump_ns", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        ns = arg.strip()
        if not ns:
            print("Usage: dump_ns <namespace>")
            return
        for sym in gdb.lookup_global_symbol(ns, gdb.SYMBOL_NAMESPACE):
            try:
                val = gdb.parse_and_eval(sym.name)
                print(f"{sym.name} = {val}")
            except:
                print(f"{sym.name} = <cannot evaluate>")


DumpNamespace()


class CallAndDumpGlobals(gdb.Command):
    """Call a function and dump a list of global variables."""

    def __init__(self):
        super(CallAndDumpGlobals, self).__init__("call_and_dump", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        args = arg.split()
        if len(args) == 0:
            print("Usage: call_and_dump <function_to_call>")
            return

        func_expr = args[0]
        print(f"Calling function: {func_expr}")
        try:
            gdb.execute(f"call {func_expr}")
        except gdb.error as e:
            print(f"Error calling function: {e}")
            return

        # List of globals to inspect
        globals_to_dump = [
            "matchers::any",
            "matchers::is_arithmetic",
            "matchers::is_integral",
            "matchers::is_lvalue",
            "matchers::is_rvalue",
            "matchers::is_ref",
            "matchers::is_pointer",
            "matchers::is_floating",
            "matchers::is_unscoped",
            "matchers::is_array",
            "matchers::is_const",
            "matchers::is_volatile",
            "matchers::is_const_lvalue",
        ]

        print("\n=== Global variables after call ===")
        for name in globals_to_dump:
            try:
                val = gdb.parse_and_eval(name)
                print(f"{name} = {val}")
            except gdb.error as e:
                print(f"{name} = <error: {e}>")


CallAndDumpGlobals()
