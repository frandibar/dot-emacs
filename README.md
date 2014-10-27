# Installation

Plugins must be installed first, for that purpose I use Cask.

Install Cask on OSX using brew:
```
$ brew install cask
```

Install Emacs plugins:
```
$ cask install
```

Install Python extensions:
```
$ pip install flake8
$ pip install ipython
$ pip install jedi
$ pip install elpy
$ pip install pep8
$ pip install rope
```

# Misc

Some **keybindings** worth mentioning:

### Searching
Show all occurrences and jump between them with cursor keys.
I used to do this with `M-x` `occur`.<br/>
`M-i` (helm-swoop)
If typed again, invokes `helm-multi-swoop`. Additionally, works during `isearch`.
While in `helm-swoop` press `C-c` `C-e` to edit mode, apply changes to original buffer by `C-x` `C-s`.


### Switching buffers
`Command-t` (helm-for-files)<br/>
Jump between buffer configurations:<br/>
`*[mode]` to filter my mode

`C-c` `<left>`/`<right>` (winner-undo)/(winner-redo)

### Jumping
`Command-i` (imenu) Fast jump to function definitions<br/>
`M-x` `point-to-register`<br/>
`M-x` `jump-to-register`<br/>

### Info
Show function definitions on another buffer (python-mode only):<br/>
`C-c` `C-o` (elpy-occur-definitions) <br/>

### Projects
`C-c` `C-p` `a` Run awk in project.<br/>
`C-c` `C-p` `h` Open file in project.<br/>

### Directory
`F8` `neotree-toggle`

### Selection
`C-=` (er/expand-region)

