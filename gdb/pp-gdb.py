#!/usr/bin/env python

import gdb.printing


class displayable_pp(gdb.ValuePrinter):
    def __init__(self, val):
        self.val = val

    def to_string(self):
        # Call the program's to_string() at debug time
        if self.val.type.code == gdb.TYPE_CODE_PTR:
            if int(self.val) == 0:
                return "nullptr"

            try:
                repr = gdb.parse_and_eval(
                    f"({self.val.address})->repr()"
                ).format_string()
            except gdb.error:
                print("shit")
                return f"<cannot call repr() on {self.val.type}>"

        try:
            repr = gdb.parse_and_eval(
                f"(({self.val.type}*){self.val.address})->repr()"
            ).format_string()
        except gdb.error:
            print("shit")
            return f"{self.val.type}(...)"

        return repr[1 : len(repr) - 1]

    def display_hint(self):
        return "string"


def build_pp():
    pp = gdb.printing.RegexpCollectionPrettyPrinter("cmm")

    pps = (
        "cmm::token",
        "cmm::location",
        "cmm:type",
    )

    pp.add_printer("shared_ptr", "^std::shared_ptr<const cmm::type>.*$", displayable_pp)

    for name in pps:
        pp.add_printer(name, f"^{name}", displayable_pp)
        pp.add_printer(f"{name}_pointer", r"^(const )?{name} \*$", displayable_pp)

    return pp
    # pp.add_printer(
    #     "std::shared_ptr<const cmm::type>",
    #     r"^std::shared_ptr<const cmm::type>$",
    #     make_pp(True),
    # )


gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pp(), replace=True)


# def fn(val):
#     lookup_tag = val.type.tag
#     if lookup_tag is None:
#         print("YEEP")
#         return None
#
#     if lookup_tag.starts_with("cmm::token *"):
#         return displayable_pp
#
#     return None
#
#
# gdb.printing.register_pretty_printer(gdb.current_objfile(), fn, replace=True)
