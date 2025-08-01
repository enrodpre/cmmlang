#!/usr/bin/env python
import pytest
import sys
from io import StringIO

# ANSI color codes
GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
RESET = "\033[0m"

# Store results for final summary
test_results = []
original_stdout = sys.stdout


def color_str(s: str, c: str) -> str:
    return f"{c}{s}{RESET}"


def pytest_runtest_logreport(report):
    """Hook called for each test report (setup, call, teardown)"""
    # Only process the 'call' phase (actual test execution)
    if report.when == "call":
        # Extract filename from test node id
        if hasattr(report, "nodeid") and "[" in report.nodeid:
            # Extract parameter from nodeid like "test_file.py::test_func[filename.txt]"
            param_start = report.nodeid.find("[") + 1
            param_end = report.nodeid.find("]")
            filename = report.nodeid[param_start:param_end]

            if report.passed:
                status = GREEN, "PASSED"
            elif report.failed:
                status = RED, "FAILED"
            elif report.skipped:
                status = YELLOW, "SKIPPED"

            color, status_str = status
            print(color_str(f"{status_str}: {filename}", color))
            # Store for summary
            test_results.append((filename, report.outcome))


def pytest_runtest_setup(item):
    """Suppress output during test setup"""
    sys.stdout = StringIO()


def pytest_runtest_call(item):
    """Suppress output during test call"""
    sys.stdout = StringIO()


def pytest_runtest_teardown(item, nextitem):
    """Suppress output during test teardown"""
    sys.stdout = StringIO()


def pytest_terminal_summary(terminalreporter, exitstatus, config):
    """Override terminal summary to prevent default output"""
    # This prevents the default summary from being printed
    pass


def pytest_report_header(config):
    """Suppress pytest header"""
    return None


def pytest_sessionfinish(session, exitstatus):
    """Hook called after all tests complete"""
    sys.stdout = original_stdout

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
    config.option.show_capture = "no"
    sys.stdout = StringIO()


def pytest_collection_modifyitems(config, items):
    """Suppress collection output"""
    pass
