# reflex-webauthn

## Setup
This repo contains three packages: `reflex-webauthn-common`, `reflex-webauthn-frontend` and `reflex-webauthn-backend`.

An example usage of these packages is available in the `example` directory.

To add these packages to your obelisk project, follow the steps below from your obelisk project root (i.e., the folder you ran `ob init` in).

### Switch Obelisk to 8.10 version
`reflex-webauthn` uses GHC 8.10 due to its dependencies, so you will need to use the same.

Inside your obelisk project root, ensure that `.obelisk/impl/github.json` contains the following:
```nix
{
  "owner": "obsidiansystems",
  "repo": "obelisk",
  "branch": "ll/ghc8_10-wip",
  "private": false,
  "rev": "f0e87e0905ff7800e0e0dc4921a70f5aaf097235",
  "sha256": "11i6d7ps3slxrkzs1q4qm5jzhvhacmfbrcwb0xph27swir6l97n4"
}
```

### Add dependency thunk
```bash
$ mkdir dep
$ cd dep
$ git clone git@github.com:obsidiansystems/reflex-webauthn.git
$ ob thunk pack reflex-webauthn
```

The last step here (`ob thunk pack`) replaces the cloned repository with a "thunk" that contains all the information obelisk needs to fetch/use the repository when needed.

Check out `ob thunk --help` to learn more about working with thunks.

### Add packages to default.nix

Your skeleton project's `default.nix` uses the [reflex-platform project infrastructure](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix). We can use the [`packages` field](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix#L53-L58) of the project configuration to add our custom packages, and [`overrides` field](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix#L72-L87) to override packages required by reflex-webauthn, as follows:

```nix
project ./. ({ hackGet, ... }: {
  overrides = import ((hackGet ./dep/reflex-webauthn) + "/backend") { obelisk = obelisk; pkgs = pkgs; };
  packages = {
    reflex-webauthn-common = (hackGet ./dep/reflex-webauthn) + "/common";
    reflex-webauthn-frontend = (hackGet ./dep/reflex-webauthn) + "/frontend";
    reflex-webauthn-backend = (hackGet ./dep/reflex-webauthn) + "/backend";
    ... # other configuration goes here
  };
})
```

Be sure to add `hackGet` to the list of items to bring into scope. `hackGet` is a nix function defined in reflex-platform that takes a path that points to either a source directory or a packed thunk (in other words, it takes a path to a thunk but doesn't care whether it's packed or unpacked). It produces a path to the source (unpacked if necessary). Once we've got that path, we just need to append the subdirectory paths to the individual repos contained in this repository.

### Add packages to cabal files

#### common/common.cabal
Add `reflex-webauthn-common` to the `build-depends` field of `common/common.cabal` in `library` stanza.
```cabal
library
  ... 
  build-depends:
    ...
    , reflex-webauthn-common
    ...
  ...
```

#### frontend/frontend.cabal
Add `reflex-webauthn-common` and `reflex-webauthn-frontend` to the `build-depends` field of `frontend/frontend.cabal` in `library` stanza.
```cabal
library
  ...
  build-depends: 
    ...
    , reflex-webauthn-common
    , reflex-webauthn-frontend
    ...
  ...
```

#### backend/backend.cabal
```cabal
library
  ...
  build-depends:
    ...
    , reflex-webauthn-backend
    , webauthn
    ...
  ...
```
Finally, add `reflex-webauthn-backend` and `webauthn` to the `build-depends` field of the library stanza in `backend/backend.cabal`.

## Adding WebAuthnRoute to your own routes (Common.Route)

### Add a route for webauthn in your backend routes
```haskell
data BackendRoute :: * -> * where
  ...
  BackendRoute_WebAuthn :: BackendRoute (R WebAuthnRoute)
  ...
```

### Add handling for this new route in your backend encoder
```haskell
...
yourOwnEncoder $ \case
  ...
  BackendRoute_WebAuthn -> PathSegment webAuthnBaseUrl webauthnRouteEncoder)
  ...
...
```

## Frontend
On the frontend, use `setupRegisterWorkflow` and `setupLoginWorkflow` like the [example](https://github.com/obsidiansystems/reflex-webauthn/blob/main/example/frontend/src/Frontend.hs#L53).
```haskell
do
  -- Create an input element for receiving username
  textDyn <- _inputElement_value <$> inputElement (def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      .~ "placeholder" =: "Enter username"
    )

  -- Create two buttons, one for login, one for registration
  (eRegister, eLogin) <- el "div" $ do
    (e1, _) <- el' "button" $ text "Register"
    (e2, _) <- el' "button" $ text "Login"
    pure (e1, e2)
  let
    textRegisterEv = tag (current textDyn) $ domEvent Click eRegister
    textLoginEv = tag (current textDyn) $ domEvent Click eLogin

  registerEv <- setupRegisterWorkflow webAuthnBaseUrl textRegisterEv
  loginEv <- setupLoginWorkflow webAuthnBaseUrl textLoginEv

  let
    greenText = divClass "correct" . text
    redText = divClass "error" . text . pack . show
    finalEv = either redText greenText <$> leftmost [registerEv, loginEv]

  void $ el "h1" $ widgetHold blank finalEv
```

## Backend
On the backend, use `withWebAuthnBackend` to match webauthn routes.
```haskell
import qualified Crypto.WebAuthn as WA
...

...
backend = Backend
  { _backend_run = withWebAuthnBackend $ \webAuthnRouteHandler -> \case
      ...
      BackendRoute_WebAuthn :/ webAuthnRoute -> webAuthnRouteHandler modifyRegCredOpts modifyLoginCredOpts webAuthnRoute
      ...
  , _backend_routeEncoder = ...
  }
  where
    modifyRegCredOpts :: ModifyCredentialOptionsRegistration
    modifyRegCredOpts regCredOpts = regCredOpts
      { WA.corRp =
        (WA.corRp regCredOpts)
          { WA.creName = "Example"
          }
      }

    modifyLoginCredOpts :: ModifyCredentialOptionsAuthentication
    modifyLoginCredOpts loginCredOpts = loginCredOpts { WA.coaTimeout = Just $ WA.Timeout 10000 }
```
