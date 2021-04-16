[[ "$(tty)" = "/dev/tty1" ]] && pgrep xmonad || startx "~/.xinitrc"

export EDITOR=nvim

[[ -d ~/.bin ]] && PATH=~/.bin:$PATH
[[ -d ~/.local/bin ]] && PATH=~/.local/bin:$PATH
[[ -d ~/.yarn/bin ]] && PATH=~/.yarn/bin:$PATH
[[ -f ~/.cargo/env ]] && . ~/.cargo/env

[[ -f ~/.config/bashrc ]] && . ~/.bashrc
