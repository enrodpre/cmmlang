#!/usr/bin/env python

import gdb
import gdb.printing


class token_pp:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        namespace_type = self.val["type"]["m_value"]
        type = str(namespace_type).split("::")[-1]
        value = self.val["value"]
        if len(str(value)) > 0:
            return f"token({type}, {value})"

        return f"token({type})"

    def display_hint(self):
        return "map"


class location_pp:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        # Call method with explicit casting
        if self.val.address is None or self.val.address == "":
            return

        cmd = f"(({self.val.type}*){self.val.address})->format()"
        result = gdb.parse_and_eval(cmd)

        # Handle different return types
        if result.type.code == gdb.TYPE_CODE_PTR:
            return result.string()  # C string
        else:
            # return str(result).replace('"', "")  # Other types
            return result

    def display_hint(self):
        return "string"


def disable_types(types):
    print(gdb.pretty_printers)
    for pp in gdb.pretty_printers:
        for disabled_type in types:
            if hasattr(pp, "name") and disabled_type in pp.name:
                pp.enabled = False


class ignore_child_pp:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return f"{self.val.type}"

    def children(self):
        """Generic filter that removes cmm::formattable from any type"""
        for field in self.val.type.fields():
            field_val = self.val[field]

            # Skip any field that contains 'formattable'
            if (field.name and "formattable" in field.name) or (
                "formattable" in str(field.type)
            ):
                continue

            if field.is_base_class:
                yield field.name or f"<{field.type}>", field_val
            else:
                yield field.name, field_val


def build_pp(pps):
    pp = gdb.printing.RegexpCollectionPrettyPrinter("cmm")
    for name, pp_cls in pps:
        pp.add_printer(name, f"^{name}$", pp_cls)
    # pp.add_printer("cmm_types", r"^cmm::.*", ignore_child_pp)
    return pp


pps = (("cmm::token", token_pp), ("cmm::location", location_pp))

gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pp(pps), replace=True)
# disabled_types = ("cmm::formattable",)
# disable_types(disabled_types)
