# emacs_config


## How to use

* Making virtualenv base you want use
  * 3.6.5 in my case(May 2019)

* Install python libraries first.

```
pip install -r python_debug.txt
```

* Install D2Coding font

```
brew install homebrew/cask-fonts/font-d2coding
```

* Install

```
mv ~/.emacs.d ~/.emacs.d.backup
ln -s .emacs.d ~/
```

And then start emacs

* Configuration for elpy

```
M-x elpy-config
--> Pyvenv -> Pyvenv Active --> Choose your virtualenv
```
Then emacs will copy your base venv directory to emacs local directory

* Install Jedi server

```
M-x jedi:install-server
```
