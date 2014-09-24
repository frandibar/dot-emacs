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

Some **keybindings** worth mentioning:

### Searching
Show all occurrences and jump between them with cursor keys.
I used to do this with occur.<br/>
`M-i` (helm-swoop)

### Switching buffers
`M-t` (helm-for-files)<br/>
Jump between buffer configurations:<br/>
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

