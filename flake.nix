{
    description = "The official hy conftest, as a pytest plugin!";
    inputs = rec {
        settings.url = github:sylvorg/settings;
        flake-utils.url = github:numtide/flake-utils;
        flake-compat = {
            url = "github:edolstra/flake-compat";
            flake = false;
        };
    };
    outputs = inputs@{ self, flake-utils, settings, ... }: with builtins; with settings.lib; with flake-utils.lib; settings.mkOutputs {
        inherit inputs;
        pname = "pytest-hy";
        callPackage = { stdenv
            , buildPythonPackage
            , pythonOlder
            , pname
        }: j.mkPythonPackage self.pkgs.${stdenv.targetPlatform.system}.Pythons.${self.type}.pkgs (rec {
            owner = "syvlorg";
            version = "0.0.1";
            inherit pname;
            disabled = pythonOlder "3.7";
            src = ./.;
            postPatch = ''substituteInPlace setup.py --replace "install_requires=[\"pytest\", \"hy\"]," ""'';
            meta = {
                description = "The official hy conftest, as a pytest plugin!";
                license = licenses.mit;
            };
        });
        type = "hy";
    };
}
