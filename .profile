[[ "$(tty)" = "/dev/tty1" ]] && pgrep xmonad || startx "~/.xinitrc"

[[ -f ~/.bin ]] && PATH=~/.bin:$PATH
[[ -f ~/.local/bin ]] && PATH=~/.local/bin:$PATH
[[ -f ~/.yarn/bin ]] && PATH=~/.yarn/bin:$PATH
[[ -f ~/.cargo/env ]] && . ~/.cargo/env

[[ -f ~/.config/bashrc ]] && . ~/.bashrc

export LANG=en_US.utf8
export EDITOR=nvim
