#!/usr/bin/env python

import gdb
import gdb.printing
from gdb.FrameDecorator import FrameDecorator
import itertools


# class FilterFrameDecorator(FrameDecorator):
#     def __init__(self, fobj):
#         super(FilterFrameDecorator, self).__init__(fobj)
#
#     def function(self):
#         frame = self.inferior_frame()
#         if frame.filename in self.filenames:
#             return ""
#
#         else
#         name = str(frame.name())
#
#         if frame.type() == gdb.INLINE_FRAME:
#             name = name + " [inlined]"
#
#         return "aaa"


class FilteredBacktrace:
    """Backtrace excluding frames from a specific source file."""

    def __init__(self):
        self.name = "CmmBacktraceFilter"
        self.priority = 100
        self.enabled = True
        self.filenames = ("visitor.hpp", "visitor.cpp")
        gdb.frame_filters[self.name] = self

    def filter(self, frame_iter):
        return filter(
            lambda frame: all(
                filename not in str(frame.filename) for filename in self.filenames
            ),
            frame_iter,
        )

    # def invoke(self, arg, from_tty):
    #     exclude_file = arg.strip()
    #     if not exclude_file:
    #         print("Usage: bt_exclude <filename>")
    #         return
    #
    #     frame = gdb.newest_frame()
    #     while frame:
    #         sal = frame.find_sal()
    #         if sal.symtab:
    #             filename = sal.symtab.filename
    #             if exclude_file not in filename:
    #                 print(frame)
    #         else:
    #             # Could be an external frame (like a system call)
    #             print(frame)
    #         frame = frame.older()


# FilteredBacktrace()
