# About
`tailwind-minor-mode` provides a complete-at-point function for the tailwind class completion inside a class attribute.

# Installation
1) clone package somewhere

``` sh
git clone https://github.com/shadr/tailwind-minor-mode.git ~/tailwind-minor-mode
```

2) add `tailwind-minor-mode` directory to the load-path of Emacs and load the package

``` emacs-lisp
(add-to-list 'load-path "~/tailwind-minor-mode/")
(require 'tailwind-minor-mode)
```

# Why not lsp-tailwindcss ?
While lsp-tailwindcss is a great package that provides a client to the tailwindcss-lsp, it needs to parse insanely huge json that tailwindcss-lsp spits out when requesting completion candidates. 

Instead of doing a request each time you need a completion suggestions, `tailwind-minor-mode` caches completion candidates as if you tried to get them for the class attribute in `<div class=""></div`.
It is done by spawning a `tailwindcss-language-server` using built-in `jsonrpc` functionality, sending a few requests to initialize workspace, open virtual file and then get all possible completions.
