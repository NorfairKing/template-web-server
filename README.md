# Web Server Template

This is a template implementation of a Yesod Web Server.

* Haskell code for a Yesod Web Server with all best practices
* Integration tests for each handler
* OptParse for the Web server
* Coverage report
* A Cabal build
* A Stack build
* A Nix build
* A Statically-linked Nix build
* An example cast for documentation
* Flake-based CI
* Pre-commit hooks

## License

This template is **not** free to use.
See https://template.cs-syd.eu/template/NorfairKing/template-web-server for more information.

Copyright (c) 2020-2024 Tom Sydney Kerckhove.

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
rm -rf *.nix nix
```

The project overlay is defined in `nix/overlay.nix`.


### Workflow examples

#### Adding a Handler

1. Add the route in `foo-bar-web-server/routes.txt`.

2. Add the handler in a new module `Foo.Bar.Web.Server.Handler.<HandlerName>` with a function according to the route definition:

   ```
   get<HandlerName> :: Handler Html
   ```

4. Add tests in `Foo.Bar.Web.Server.Handler.<HandlerName>Spec`.

