import subprocess
from pathlib import Path

import pytest

TESTDIR = Path("/home/kike/dev/cmm/build/test")
COMPILER_NAME = "CmmLang"
RESOURCES_PATH = TESTDIR / "examples"
GENERATED_PATH = TESTDIR / "generated"
OK_STATUS = 255


def get_examples():
    return (name for name in RESOURCES_PATH.iterdir() if name.is_file())


def compile(filename: Path):
    if not Path(COMPILER_NAME).exists():
        pytest.fail(f"Binary not found {COMPILER_NAME}")

    output_path = GENERATED_PATH / filename.stem
    compilation = [COMPILER_NAME, "-o", str(output_path), str(filename)]
    try:
        res = subprocess.run(compilation, text=True, capture_output=True, timeout=3)
    except subprocess.TimeoutExpired:
        pytest.fail(f"Binary timed out when processing {filename}")
    except Exception as e:
        pytest.fail(f"Failed to execute binary with {filename}: {e}")

    if res.returncode != 0:
        pytest.fail(f"Compilation not successfull. {res.stderr}")

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
    split = path.parts
    if len(split) == 3:
        return split[1]
    return OK_STATUS


@pytest.mark.parametrize("filename", testfiles, ids=get_filename)
@pytest.mark.timeout(3)
def test_case(filename: Path, request):
    compiled_file = compile(filename)
    assert compiled_file.exists()

    actual_status = run(compiled_file)
    assert OK_STATUS == actual_status
