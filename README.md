# ctune: Tune out CC Noise Macros

> Minor mode to help CC Mode get the right indentation and fontification,
by specifying Noise Macros.

## Table of Contents

- [Installation](#installation)
- [Using](#using)
- [License](#license)


---

## Example

With point at a Noise Macro, type:
`C-c C-#`
and keep editing, without that Noise Macro interfering.

---

## Installation

- Download and open up a terminal in ctune dir.
- `make`
- Add the directory where ctune is to your load path.

Done!

- To build the Info file, type: make info

Other way to build the file is to execute:
`emacs -batch -f batch-byte-compile ctune.el`

- If you want the HTML version of the manual, type `make html`.

---

## Using

Require the file in your .init file:
(require 'ctune)

To activate the minor mode, either type `M-x ctune-mode`, or eval
`(ctune-mode 1)`

When editing a C buffer with Noise Macros, type `C-c C-#` to tell CC
Mode that the identifier at point is a Noise Macro.
Optionally save the values accumulated to the .dir-locals.el file,
with `M-x ctune-save-noise-macros`.

Read the ctune.info manual, for more.

---

## License

- **[GPL 3](https://www.gnu.org/licenses/gpl-3.0-standalone.html)**
- Copyright 2019 Mauro Aranda
