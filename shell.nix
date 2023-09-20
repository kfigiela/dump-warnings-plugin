with import ./default.nix { };
let
  python-commit-hooks = with pkgs.python3Packages;
    buildPythonPackage rec {
      pname = "pre-commit-hooks";
      src = sources.python-commit-hooks;
      version = src.tag;
      propagatedBuildInputs = [ ruamel-yaml tomli ];
      doCheck = false;
    };
  pre-commit-config = {
    src = ./.;
    tools = pkgs; # // { inherit fourmolu; };
    hooks = {
      hpack-deep = {
        enable = true;
        name = "hpack-deep";
        entry = "${pre-commit.tools.hpack-dir}/bin/hpack-dir --silent";
        files = "(\\.l?hs$)|(\\.cabal$)|(package.yaml$)";
      };
      fourmolu.enable = true;
      nixfmt = {
        enable = true;
        excludes = [ "nix/sources\\.nix" ];
      };
      trailing-whitespace = {
        enable = true;
        name = "trailing-whitespace";
        entry = "${python-commit-hooks}/bin/trailing-whitespace-fixer";
        types = [ "text" ];
        excludes = [ ];
      };
      end-of-file = {
        enable = true;
        name = "end-of-file";
        entry = "${python-commit-hooks}/bin/end-of-file-fixer";
        types = [ "text" ];
        excludes = [ ];
      };
      byte-order-marker = {
        enable = true;
        name = "byte-order-marker";
        entry = "${python-commit-hooks}/bin/fix-byte-order-marker";
        types = [ "text" ];
      };
    };
  };

  pre-commit = import "${sources.pre-commit}/nix" {
    inherit (sources) nixpkgs;
    gitignore-nix-src = system.gitignore;
  };
  pre-commit-check = pre-commit.run pre-commit-config;

in haskellPkgs.shellFor {
  packages = p: [ p.dump-warnings-plugin ];
  nativeBuildInputs = with pkgs; [
    cabal-install
    haskellPkgs.haskell-language-server
    hpack
    jq
  ];

  shellHook = ''
    { ${pre-commit-check.shellHook} } 2> /dev/null
    if [ -f .envrc ] && [ -f .git/hooks/pre-commit ]; then
      grep --silent "direnv export bash" .git/hooks/pre-commit || sed --in-place '2 i eval "$(direnv export bash)"' .git/hooks/pre-commit
    fi
  '';
}
