import os
import sys
from functools import reduce
from operator import or_
from pathlib import Path

import pytest

import hy
from hy._compat import PY3_8, PY3_10

NATIVE_TESTS = Path.cwd().joinpath("tests", "native_tests")

# https://github.com/hylang/hy/issues/2029
os.environ.pop("HYSTARTUP", None)

def pytest_ignore_collect(path, config):
    versions = [
        (sys.version_info < (3, 8), "sub_py3_7_only"),
        (PY3_8, "py3_8_only"),
        (PY3_10, "py3_10_only"),
    ]
    return reduce(or_, (name in path.basename and not condition for condition, name in versions)) or None

def pytest_collect_file(parent, path):
    if (
        path.ext == ".hy"
        and (
            path.basename.startswith("test_")
            or path.basename.startswith("test-")
            or path.basename.endswith("_test")
            or path.basename.endswith("-test")
        )

        # Mimics https://github.com/hylang/hy/blob/master/conftest.py#L38:
        and ((not NATIVE_TESTS.exists()) or (NATIVE_TESTS.exists() and (str(NATIVE_TESTS) in (path.dirname + os.sep))))

        and path.basename != "__init__.hy"
    ):
        return pytest.Module.from_parent(parent, path=Path(path))