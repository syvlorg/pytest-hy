import os
from functools import reduce
from operator import or_
from pathlib import Path

import pytest

import hy
from hy._compat import PY3_8, PY3_10

NATIVE_TESTS = Path.cwd().joinpath("tests", "native_tests")

# https://github.com/hylang/hy/issues/2029
os.environ.pop("HYSTARTUP", None)

def pytest_collect_file(parent, path):
    if (
        path.ext in (".hy", ".py")
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
        path = Path(path)
        path.touch(exist_ok = True)
        return pytest.Module.from_parent(parent, path = path)
