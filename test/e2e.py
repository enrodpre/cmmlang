import os
import subprocess
from pathlib import Path

import pytest

COMPILER_NAME = "CmmLang"
RESOURCES_PATH = Path("examples")
GENERATED_PATH = Path("generated")
OK_STATUS = 255


def get_examples():
    return (name for name in RESOURCES_PATH.iterdir() if name.is_file())


def compile(filename: Path):
    output_path = GENERATED_PATH / filename.stem
    compile_command = "{} -o {} {}".format(COMPILER_NAME, output_path, filename)
    print("Compiling with {}".format(compile_command))
    res = subprocess.run(compile_command, shell=True, capture_output=True, text=True)
    return output_path


def run(filename: Path):
    run_command = "{}/{}".format(GENERATED_PATH, filename.stem)
    print("Running with {}".format(run_command))
    res = subprocess.run(run_command, shell=True, capture_output=True, text=True)
    return res.returncode


testfiles = get_examples()


def get_filename(path):
    return Path(path).name


def get_ok_code(path) -> int:
    splitted = path.parts
    if len(splitted) == 3:
        return splitted[1]
    return OK_STATUS


@pytest.mark.parametrize("filename", testfiles, ids=get_filename)
@pytest.mark.timeout(3)
def test_case(filename, request):
    ok_code = get_ok_code(filename)
    print("Expecting {}".format(ok_code))
    compiled_file = compile(filename)
    assert compiled_file.exists()

    actual_status = run(compiled_file)
    request.node._exit_code = actual_status
    assert OK_STATUS == actual_status
