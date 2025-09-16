# About
`tailwind-minor-mode` provides a complete-at-point function for the tailwind class completion inside a class attribute.

# Installation
1) clone package somewhere

`git clone https://github.com/shadr/tailwind-minor-mode.git ~/tailwind-minor-mode`

2) add `tailwind-minor-mode` directory to the load-path of Emacs and load the package

``` emacs-lisp
(add-to-list 'load-path "~/tailwind-minor-mode/")
(require 'tailwind-minor-mode)
```

# Why not lsp-tailwindcss ?
While lsp-tailwindcss is a great package that provides client to the tailwindcss-lsp, it needs to parse insanely huge json that tailwindcss-lsp spits out when requesting completion candidates. 

Instead of requesting completion from the lsp, this package reads tailwind classes from a pregenerated file.
Downside of this approach that we loose user defined classes in `input.css`/`tailwind.config.js`.

# Tailwind keywords file
`tailwind_keywords.txt` consists of rows in format "class kind" where:

- class - name of the tailwind class
- kind - completion kind of the class (color, enum, constant etc)

It is generated manually using a function from `copy_candidates.el`
