{
    description = "The official hy conftest, as a pytest plugin!";
    inputs = rec {
        settings = {
            url = github:sylvorg/settings;
            inputs.py3pkg-pytest-hy.follows = "";
        };
        # nixpkgs.follows = "settings/nixpkgs";
        nixpkgs.url = github:nixos/nixpkgs/nixos-22.05;
        flake-utils.url = github:numtide/flake-utils;
        flake-compat = {
            url = "github:edolstra/flake-compat";
            flake = false;
        };
    };
    outputs = inputs@{ self, flake-utils, settings, ... }: with builtins; with settings.lib; with flake-utils.lib; settings.mkOutputs {
        pname = "pytest-hy";
        callPackage = { lib, buildPythonPackage, fetchFromGitHub, pythonOlder, pytest, hy, pname }: let
            owner = "syvlorg";
        in buildPythonPackage rec {
            version = "0.0.1";
            inherit pname;
            disabled = pythonOlder "3.7";
            src = ./.;
            buildInputs = [ pytest hy ];
            meta = with lib; {
                homepage = "https://github.com/${owner}/${pname}";
                description = "The official hy conftest, as a pytest plugin!";
                license = licenses.mit;
            };
        };
        python = "hy";
    };
}
