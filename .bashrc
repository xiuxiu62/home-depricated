[[ $- != *i* ]] && return

export HISTCONTROL=ignoreboth:erasedups
export EDITOR=nvim
export PS1="\[$(tput bold)\]\[\033[38;5;9m\][\[$(tput sgr0)\]\[\033[38;5;220m\]\u\[$(tput sgr0)\]\[\033[38;5;76m\]@\[$(tput sgr0)\]\[\033[38;5;81m\]\h\[$(tput sgr0)\] \[$(tput sgr0)\]\[$(tput bold)\]\[\033[38;5;205m\]\W\[$(tput sgr0)\]\[\033[38;5;9m\]]\[$(tput sgr0)\]\\$\[$(tput sgr0)\] \[$(tput sgr0)\]"

# Includes
[[ -d ~/.bin ]] && PATH=~/.bin:$PATH
[[ -d ~/.local/bin ]] && PATH=~/.local/bin:$PATH
[[ -d ~/.yarn/bin ]] && PATH=~/.yarn/bin:$PATH
[[ -f ~/.cargo/env ]] && . ~/.cargo/env
[[ -f ~/.aliases ]] && . ~/.aliases

# Bad hack to set default cursor
xsetroot -cursor_name left_ptr

bind "set completion-ignore-case on"
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases

# Function for extracting various compression schemes
ex ()
{
	if [ -f $1 ] ; then
		case $1 in
			*.tar.bz2)   tar xjf $1   ;;
			*.tar.gz)    tar xzf $1   ;;
			*.bz2)       bunzip2 $1   ;;
			*.rar)       unrar x $1   ;;
			*.gz)        gunzip $1    ;;
			*.tar)       tar xf $1    ;;
			*.tbz2)      tar xjf $1   ;;
			*.tgz)       tar xzf $1   ;;
			*.zip)       unzip $1     ;;
			*.Z)         uncompress $1;;
			*.7z)        7z x $1      ;;
			*.deb)       ar x $1      ;;
			*.tar.xz)    tar xf $1    ;;
			*.tar.zst)   unzstd $1    ;;      
			*)           echo "'$1' cannot be extracted with this function" ;;
		esac
	else
		echo "'$1' is not a valid file"
	fi
}

# Git hacking
git()
{
	if [ $# -gt 0 ] ; then
	   if [ "$1" == "diff" ] ; then
		   shift
		   command git diff --color "$@"
	   elif [ "$1" == "status" ] ; then
		   shift
		   command git status --porcelain -b "$@"
	   else
		   command git "$@"
	   fi
	fi
}
