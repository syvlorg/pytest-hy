from setuptools import setup

setup(
    name="pytest-hy",
    version="1.0.0.0",
    author="Jeet Ray",
    author_email="jeet.ray@syvl.org",
    description="A combination of the most recent hylang-based conftests!",
    url="https://github.com/syvlorg/pytest-hy",
    packages=["pytest_hy"],
    # the following makes a plugin available to pytest
    entry_points={"pytest11": ["hy.pytest_plugin = pytest_hy.conftest"]},
    # custom PyPI classifier for pytest plugins
    classifiers=[
        "Framework :: Pytest",
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires=["pytest", "hy"],
    python_requires=">=3.7",
)
