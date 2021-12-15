final: previous:
with final.haskell.lib;

{
  fooBarPackages =
    let
      fooBarPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              buildStrictly (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
      fooBarPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (fooBarPkg name);
      fooBarPkgWithOwnComp = name: fooBarPkgWithComp name name;

    in
    {
      "foo-bar-web-server" = fooBarPkg "foo-bar-web-server";
    };

  fooBarRelease =
    final.symlinkJoin {
      name = "foo-bar-release";
      paths = builtins.map justStaticExecutables (final.lib.attrValues final.fooBarPackages);
    };


  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                final.fooBarPackages
            );
      }
    );
}
