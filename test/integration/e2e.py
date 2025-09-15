import subprocess
from pathlib import Path

import pytest

SRCDIR = Path("/home/kike/dev/cmm")
BUILDIR = SRCDIR / "build"
CWD = BUILDIR / "test/integration"
COMPILER_NAME = BUILDIR / "CmmLang"
RESOURCES_PATH = SRCDIR / "examples"
GENERATED_PATH = CWD / "generated"
OK_STATUS = 255


def get_examples():
    return (name for name in RESOURCES_PATH.iterdir() if name.is_file())


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
    if not filename.exists():
        pytest.skip()

    if not Path(COMPILER_NAME).exists():
        pytest.fail(f"Binary not found {COMPILER_NAME}")

    output_path = GENERATED_PATH / filename.stem
    compilation = [str(COMPILER_NAME), "-o", str(output_path), str(filename)]
    try:
        res = subprocess.run(compilation, text=True, capture_output=True, timeout=3)
    except subprocess.TimeoutExpired:
        pytest.fail(f"Binary timed out when processing {filename}")
    except Exception as e:
        pytest.fail(f"{e}")

    if res.returncode != 0:
        request.node.user_properties.append(("stderr", res.stderr.strip()))
        pytest.fail(res.stderr)

    assert output_path.exists()

    result = subprocess.run(
        GENERATED_PATH / filename.stem, capture_output=True, text=True
    )
    actual_status = result.returncode
    assert OK_STATUS == actual_status
