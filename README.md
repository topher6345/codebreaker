# Codebreaker

in Elm

## Build

```sh
elm make --debug src/Main.elm --output build/main.js
```

## Serve

```sh
elm reactor
```

## Test

```sh
elm-test
```

## Deploy
```sh
elm make src/Main.elm --optimize --output build/main.js && gh-pages -d .
```