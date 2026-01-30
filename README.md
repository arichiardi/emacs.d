# Emacs

This repo started as a fork of [Emacs Live](https://github.com/overtone/emacs-live) but has slowly drifted away from it, mainly cause moving a bit faster than the original.

One of the things this fork does is that it avoids committing the `packs/stable` folder so every time you make a change to `packs/dev` be sure you execute: `packs/update-live-packs`.

The philosophy here is to have a stable but rolling, contributor friendly, setup (especially regarding `clojure-pack` and `cider`, which is the one I use the most) and will slowly move to [borg](https://github.com/emacscollective/borg) for package management.

## First time

After cloning the project, you need to sync the submodules and run a couple of scripts for compiling (and therefore have faster startup):

```
git submodule update --init --recursive
./packs/update-live-packs

make init-clean && make init-build
```

If you get lost in submodule mess, your last resort can be something like: 

```
git submodule update --recursive --checkout --force
```

This will force the checkout of the submodules at the SHA stored in this parent repository.

### Build assimilated modules

> [!WARNING]  
> This list will likely be outdated over time as it depends on the assimilated modules.

`make build/olivetti build/marginalia build/vertico build/flycheck build/clj-refactor`

### Original Emacs Live Disclaimer

An opinionated set of defaults for getting started with a specific focus
on live coding with [Overtone](http://overtone.github.io) and
[Quil](http://github.com/quil/quil). However, it's not just a one trick
pony. It also happens to be:

* a jolly good generic Clojure hacking config
* a nice structured approach to organising your Emacs config
* modular in that functionality is organised by discrete _packs_
* a decent starting point for live coding in general
* a goldmine of config snippets to plunder and add to your own config

So, wherever you are in the multiverse, Emacs Live is ready to join you
in battle against the evil friction of poor text editor workflows.

    "Power of the horse, full force!"
                 The Space Stallions.

### Requires Emacs 24.3

It is only compatible with Emacs 24.3 and above but haven't been tested on it
thoroughly.

### Getting Started

The only way to install is to follow these steps:

```shell
curl -O https://raw.githubusercontent.com/arichiardi/clojure-live/master/installer/install-emacs-live.sh
chmod + install-emacs-live.sh
./install-emacs-live.sh # follow the installer
...
cd .emacs.d
./packs/compile-live-packs
./packs/update-live-packs
```

## Live Packs

Emacs Live is powered by a number of special packs. Packs are
directories which are used to store isolated sets of functionality and
configuration. These may be symlinks or git submodules depending on how
you choose to store and manage your dot emacs.

### Pack Structure

Each pack consists of three components: the `init.el`, `config` dir and
`lib` dir. The `init.el` file is loaded first and it is here that you
can run arbitrary elisp. However, it is recommended that you organise
your pack by placing library code in the lib dir and individual config
files in the config dir. Emacs Live provides helper fns to make it easy
for you to load config files and for you to add lib dirs to your load
path. See the section on helper fns below.

### Loading Packs

By default, Emacs live will load the packs in the following order:

1. `foundation-pack`
2. `colour-pack`
3. `lang-pack`
4. `power-pack`
5. `git-pack`
7. `clojure-pack`
8. `bindings-pack`

However, you may create a `~/.emacs-live.el` file to override this
behaviour. Simply set the var live-packs to a list of symbols
representing the packs you'd like to load up (the order will be
honoured). For example to only load the foundation and colour packs:

    (live-use-packs '(stable/foundation-pack stable/colour-pack))

If just you wish to load your own packs after the default packs then
simply use `live-add-packs`:

    (live-add-packs '(~/.live-packs/yourname-pack))

Packs are expected to reside in `~/.emacs.d/packs/` unless you specify
them with absolute paths in which case the absolute path with be
honoured.

### Compiling dev packs and debug

```shell
cd .emacs.d
./packs/compile-live-packs # this also accepts --magit, --cider and --clean
./packs/update-live-packs
```

The above can be done multiple times and the `echo $?` should tell you if
everything was alright.

If something goes wrong, run `emacs --debug-init` and paste in an issue the
relevant `*Messages*` buffer output.

### Creating your own Packs

Emacs Live provides a couple of useful helper fns which you can use
within your own live packs:

* `live-pack-lib-dir` this returns the path of the lib dir for the
  current pack
* `live-load-config-file` loads a config file located in the config dir
  of the current pack

It is recommended that you place your own personal packs in an external
directory. See the `user/template-pack`'s README for more information.

