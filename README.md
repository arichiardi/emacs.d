# My Emacs

This repo started as a fork of [Emacs Live](https://github.com/overtone/emacs-live) but has slowly drifted away from it, mainly cause moving a bit faster than the original.

One of the things this fork does is that it avoids committing the `packs/stable` folder so every time you make a change to `packs/dev` be sure you execute: `packs/update-live-packs`.

The philosophy here is to have a stable but rolling, contributor friendly, setup (especially regarding `clojure-pack` and `cider`, which is the one I use the most) and will slowly move to [borg](https://github.com/emacscollective/borg) for package management.

## First time

After cloning the project, you need to sync the submodules and run a couple of scripts for compiling (and therefore have faster startup):

```
cd .config
git clone git@github.com:arichiardi/emacs.d.git emacs
cd emacs
git submodule update --init --recursive
./packs/update-live-packs

make init-clean && make init-build
```

If you get lost in submodule mess, your last resort can be something like: 

```
git submodule update --recursive --checkout --force
```

This will force the checkout of the submodules at the SHA stored in this parent repository.

### Fonts

Copy the following to `/usr/share/emacs/site-lisp/site-start.el` (see [here](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html) for more details) or a custom pack:

```
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font Mono-12"))

(set-fontset-font "fontset-default"
                  'emoji
                  (font-spec :family "Noto Color Emoji"))
```

<details>
<summary>Why only <code>emoji</code> and not <code>symbol</code>?</summary>

**`'emoji`** covers the Unicode Emoji blocks (U+1F600–U+1F64F, U+1F300–U+1F5FF, etc.) — pictographic characters like 😀🎉🔥.

Emacs has a variable `use-default-font-for-symbols` that defaults to `t`. When enabled, Emacs uses the **default font** for all symbol characters instead of looking up a separate symbol fontset. Since our default font is JetBrainsMono Nerd Font — which includes extensive symbol coverage (arrows, box-drawing, math symbols, currency signs, etc.) — we don't need a separate `'symbol` entry.

We only set `'emoji` because emoji fonts are a special case: they're color bitmap fonts that your main monospace font simply won't include. Without this explicit mapping, emojis would render as tofu (□).

Note: if you ever set `(setq use-default-font-for-symbols nil)`, you'd want to add back a `'symbol` fontset entry (e.g. using Symbola or Noto Sans Symbols). Be aware there's a risk of **overreach** — an emoji font covering `'symbol` can steal rendering of Greek letters, mathematical operators, etc. from your main font.

See [Emacs Stack Exchange](https://emacs.stackexchange.com/questions/62049) and the [GNU Emacs manual on modifying fontsets](https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifying-Fontsets.html) for more details.
</details>

Alternatively you can use `$HOME/.Xresources`:

```
!! Emacs - https://www.gnu.org/software/emacs/manual/html_node/emacs/X-Resources.html
Emacs.menuBar:     0
Emacs.toolBar:     0
Emacs.font:        JetBrainsMono Nerd Font Mono-12
```

This latter method strictly depends on how you launch your X session (verify the variables are there with `xrdb -query | grep -i Emacs`).

### Build assimilated modules

> [!WARNING]  
> This will compile all the libraries, following all the specified `build-step` in `.gitmodules`, as per [Borg's official documentation](https://docs.emacsmirror.org/borg/Variables.html#index-submodule_002eDRONE_002ebuild_002dstep).

`make build`

# From Emacs Live

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

Packs are expected to reside in the emacs `packs/` sub-dir unless you specify
them with absolute paths in which case the absolute path with be
honoured.

### Creating your own Packs

Emacs Live provides a couple of useful helper fns which you can use
within your own live packs:

* `live-pack-lib-dir` this returns the path of the lib dir for the
  current pack
* `live-load-config-file` loads a config file located in the config dir
  of the current pack

It is recommended that you place your own personal packs in an external
directory. See the `user/template-pack`'s README for more information.

