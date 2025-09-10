#!/usr/bin/env python
import sys
from io import StringIO

import pytest

# ANSI color codes
ERROR = "\033[1;38;2;205;92;92m"
GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
RESET = "\033[0m"

# Store results for final summary
test_results = []
original_stdout = sys.stdout


def print_report(color, filename, duration, outcome, message=""):
    print(f"{color}{outcome}: {duration:.3f} {filename}. {message}{RESET}")


def pytest_runtest_logreport(report):
    """Hook called for each test report (setup, call, teardown)"""
    sys.stdout = original_stdout
    # Only process the 'call' phase (actual test execution)
    if report.when == "call":
        # Extract filename from test node id
        if hasattr(report, "nodeid") and "[" in report.nodeid:
            # Extract parameter from nodeid like "test_file.py::test_func[filename.txt]"
            param_start = report.nodeid.find("[") + 1
            param_end = report.nodeid.find("]")
            filename = report.nodeid[param_start:param_end]
            prop = ""
            if len(report.user_properties) > 0:
                prop = report.user_properties[0][1]

            if report.passed:
                print_report(GREEN, filename, report.duration, "PASSED")
            elif report.failed:
                print_report(ERROR, filename, report.duration, "FAILED", prop)
            elif report.skipped:
                print_report(YELLOW, filename, report.duration, "SKIPPED")

            # Store for summary
            test_results.append((filename, report.outcome))
    # sys.stdout = StringIO()


def pytest_runtest_setup(item):
    """Suppress output during test setup"""
    # sys.stdout = StringIO()
    pass


def pytest_runtest_call(item):
    """Suppress output during test call"""
    # sys.stdout = StringIO()
    pass


def pytest_runtest_teardown(item, nextitem):
    """Suppress output during test teardown"""
    # sys.stdout = StringIO()
    pass


def pytest_terminal_summary(terminalreporter, exitstatus, config):
    """Override terminal summary to prevent default output"""
    # This prevents the default summary from being printed
    pass


def pytest_report_header(config):
    """Suppress pytest header"""
    return None


def pytest_sessionfinish(session, exitstatus):
    """Hook called after all tests complete"""
    # sys.stdout = original_stdout

    if test_results:
        print("\n" + "=" * 50)
        print("INTEGRATION TEST SUMMARY")
        print("=" * 50)

        passed = sum(1 for _, outcome in test_results if outcome == "passed")
        failed = sum(1 for _, outcome in test_results if outcome == "failed")
        skipped = sum(1 for _, outcome in test_results if outcome == "skipped")

        print(
            f"Total: {len(test_results)} | {GREEN}Passed: {passed}{RESET} | "
            f"{RED}Failed: {failed}{RESET} | {YELLOW}Skipped: {skipped}{RESET}"
        )
        print("=" * 50)


def pytest_configure(config):
    """Configure pytest to suppress normal output for cleaner display"""
    # Disable normal pytest output for a cleaner look
    config.option.quiet = 2
    config.option.tb = "no"
    config.option.disable_warnings = True
    config.option.show_capture = "all"
    sys.stdout = StringIO()


def pytest_collection_modifyitems(config, items):
    """Suppress collection output"""
    pass
