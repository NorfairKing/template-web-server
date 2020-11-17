# Web Server Template

This is a template implementation of a Yesod Web Server.

* Haskell code for a Yesod Web Server with all best practices
* Integration tests for each handler
* OptParse for the Web server
* A nix build
* An example cast for documentation
* CI
  * Stack-based CI
  * Nix-based CI
* Pre-commit hooks

## License

This template is **not** free to use.
See https://template.cs-syd.eu/template/NorfairKing/template-web-server for more information.

Copyright (c) 2020 Tom Sydney Kerckhove.

All Rights Reserved.

## Instructions

To use this template in a new project, choose the name for your project, for example `homeless-shelter`.
Then use [template-filler](https://github.com/NorfairKing/template-filler) to use the template, like this:

```
template-filler --source /path/to/this/template-web-server --destination /path/to/your/homeless-shelter --find FooBar --replace HomelessShelter
```

### Template overview

There is a single Haskell package in `foo-bar-web-server`.
It contains the following structure:

- The entry point in `Foo.Bar.Web.Server`
- The Yesod `App` type in `Foo.Bar.Web.Server.Foundation`
- The Handlers in `Foo.Bar.Web.Server.Handler.<HandlerName>`
- OptParse in `Foo.Bar.Web.Server.OptParse`
- Declarations of static assets in `Foo.Bar.Web.Server.Static.TH`
- The `development` constant in `Foo.Bar.Web.Server.Constants`
- Test utilities in `Foo.Bar.Web.Server.TestUtils`
- Tests per handler `Foo.Bar.Web.Server.Handler.<HandlerName>Spec`

### OptParse

The option parsing is based on [the option parsing template](https://github.com/NorfairKing/template-optparse).
It is included in this template so you will not need to also buy the option parsing template.

For more information about how to use the option parsing, follow the instructions in `template-web-server/src/Foo/Bar/Web/Server/OptParse.hs`.

### Nix build

If you don't need a nix build, remove these files:

```
rm -rf *.nix nix .github/workflows/nix.yaml
```

In `nix/nixpkgs-version.nix`, we pin a `nixpkgs` commit.
In `nix/pkgs.nix` we define our own 'version' of the `nixpkgs` by adding our own overlays.
The project overlay is defined in `nix/overlay.nix`.

See the instructions in `nix/overlay.nix` for more details.

### CI

CI is set up for both a stack build and a nix build.
See `.github/workflows` for more details.

The stack build should "just work".

For the nix build to work, there is a manual step that you need to go through:
First, make a cachix cache at cachix.org.
Put its name in the right places within `.github/workflows/nix.yaml`.
Then put its signing key in the 'Secrets' part of your repository on github.


### Workflow examples

#### Adding a Handler

1. Add the route in `foo-bar-web-server/routes.txt`.

2. Add the handler in a new module `Foo.Bar.Web.Server.Handler.<HandlerName>` with a function according to the route definition:

   ```
   get<HandlerName> :: Handler Html
   ```

4. Add tests in `Foo.Bar.Web.Server.Handler.<HandlerName>Spec`.

