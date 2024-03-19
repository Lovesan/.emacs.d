# Personal Emacs init files

I mostly use Emacs as a Common Lisp IDE, by means of `SLIME` extension.

Probably you would require recent Emacs to utilize these configs. Emacs 29.1 would be fine.

The configuration is a bit minimalistic currently but will be expanded later.

## Packages

The configuration consists of mostly `SLIME`, and its extensions(like `ac-slime` for auto-completion), `Magit`(although completely vanilla), and several utilities here and there, like for ex. `editorconfig`.

`Neotree` is used for the side panel. It does also utilize `all-the-icons`, so you should execute `M-x all-the-icons-install-fonts` on the first startup.

`lispy` package is used for structural editing of lisp code, for both Emacs Lisp and Common Lisp.

## Configuration

Apart from modifying sources in the `lisp/` directory, you can also take a look at `init.el`, it has a few configuration options which you can uncomment.

## Keybindings

The configuration features non-standard(in Emacs terms) keybindings, to make it behave more like mainstream editors and IDEs.

It does also modify the killring behavior, for the same reasons.

The problem with default Emacs keybindings is that those were designed for ancient lisp-machine keyboards, and are not suitable for modern ones.

Having been unsatisfied with CUA-mode, I utilized `rebinder.el` library, and rebound **C-x** and **C-c** prefix keys to **C-e** and **C-d** respectively.

Here's the full list of bindings:

#### Generic stuff:

- **C-q** - exit Emacs
- **Menu/Apps** key - extended command (i.e. `M-x` (`M` is `Alt` in Emacs terminology))

#### Editing:

- **C-c** - copy
- **C-x** - cut
- **C-v** - paste
- **C-z** - undo
- **C-y** - redo
- **C-a** - select everything in the buffer

#### Buffer management:

- **C-o** - open file
- **C-s** - save buffer
- **C-w** - kill buffer (also **C-F4**)
- **M-left** - previous buffer
- **M-right** - next buffer
- **F2** - opens the list of buffers (provided by ibuffer)
- **C-PageDown** - shows `ace-window` overlay which allows for quick switching between tiled windows.
- **F8** - toggle neotree

#### Search:

- **C-f** - search forward
- **C-S-f** - search backward

You can also navigate using arrows while performing a search (i.e. search backward/forward and cycle through previous searches)

#### A note on mouse:

Mouse behavior is also modified. So, for example, buffer navigation could be done by **mouse-4** and **mouse-5** buttons, usually bound to the next and previous actions on mouse devices and drivers. So, these buttons basically cycle through next/previous buffers, with several added bits of intelligent behavior as explained below.

#### Both Emacs Lisp mode and SLIME:

- **F12** - go to definition. The same could be performed while clicking on the symbol while simultaneously holding the Control key. Most modern IDEs behave this way.
- **F11** - pop definition stack and return to the previous position. This can also be performed by **mouse-4**(i.e. `back` button).
- **mouse-5** - Go forward the definition stack, provided that the above command has been previously executed.
- **F5** - execute previous form. I.e. the one right before the caret.

#### SLIME only:

- **F1** - describe symbol(`slime-describe-symbol`, can also describe symbol under the caret).
- **F3** - SLIME apropos
- **F4** - Toggle SLIME REPL buffer. Also works outside of SLIME, and switches to the REPL buffer in the current window.
- **F6** - Compile and load file

All other keybindings are mostly unchanged(for the moment!). There are a few tweaks for the lispy mode, though (namely, `e` button is disabled. It just outputs a normal letter `e`).

Here should be some links:
- https://slime.common-lisp.dev/
- https://github.com/abo-abo/lispy
- https://magit.vc/
