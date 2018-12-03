# chat

An OTP application

## Install

```
git clone -b master --recurse-submodules git@github.com:stleon/chaterl.git
```

## Build

```
$ rebar3 compile
```

## Run

You must set **recapcha_secret** in **config/sys.config.devel** before start.

Then simple run

```
make run
```