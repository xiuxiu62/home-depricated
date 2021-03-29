[[ "$(tty)" = "/dev/tty1" ]] && pgrep xmonad || startx "~/.xinitrc"

[[ -f ~/.config/path ]] && . ~/.config/path
[[ -f ~/.config/bashrc ]] && . ~/.bashrc
[[ -f ~/.cargo/env ]] && . ~/.cargo/env

export LANG=en_US.utf8
export EDITOR=nvim
