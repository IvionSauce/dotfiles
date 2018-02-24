## Aliases ##
alias d="/bin/ls --color"
alias ls="/bin/ls --color -A"
alias ll="/bin/ls --color -hl"
alias lla="/bin/ls --color -Ahl"

alias dvd="du -cB M"
alias dvdr="growisofs -Z /dev/sr0 -r -J -D -l"

alias hmp="mpv --audio-device=alsa/crossfeed"
alias jine="LANG=ja_JP.utf8 wine"


## Misc Settings ##
# Give us pretty colours.
eval `dircolors -b /etc/DIR_COLORS`
# Moving easier around the filesystem and the directory stack.
setopt autocd autopushd pushdignoredups pushdminus
DIRSTACKSIZE=11


## Commandline editing ##

# Rebind M-q, previously was push-line.
bindkey "^[q" push-line-or-edit

# Make C-a first move to after the first word (the assumed command) for easier
# editing of arguments.
bindkey "^A" move-to-arguments-or-beginning
move-to-arguments-or-beginning() {
    if [[ $WIDGET == $LASTWIDGET ]]; then
	# Repeated calls will switch between beginning and arguments location.
	if [[ $CURSOR == 0 ]]; then
	    zle forward-word
	else
	    zle beginning-of-line
	fi
    else
	zle beginning-of-line
	zle forward-word
    fi
}
zle -N move-to-arguments-or-beginning


## History Settings ##
setopt sharehistory histignorealldups
HISTSIZE=10000
SAVEHIST=$HISTSIZE
HISTFILE=$ZDOTDIR/.zsh_history

# Make M-p/M-n complete based on what's already been typed, instead of just the
# first word (history-search-<backward/forward>).
bindkey "^[p" history-beginning-search-backward
bindkey "^[n" history-beginning-search-forward

# Make up/down and C-p/C-n traverse local history first.
# http://superuser.com/questions/446594/
bindkey "^[[A" up-line-or-local-history
bindkey "^P" up-line-or-local-history
bindkey "^[[B" down-line-or-local-history
bindkey "^N" down-line-or-local-history

up-line-or-local-history() {
    zle set-local-history 1
    zle up-line-or-history
    zle set-local-history 0
}
zle -N up-line-or-local-history

down-line-or-local-history() {
    zle set-local-history 1
    zle down-line-or-history
    zle set-local-history 0
}
zle -N down-line-or-local-history

# Make M-m cycle backwards through arguments, after having recalled a previous
# argument. (eg M-.)
bindkey "^[m" copy-earlier-word
autoload -Uz copy-earlier-word
zle -N copy-earlier-word


## Completion Settings ##
zstyle ':completion::complete:*' use-cache 1

# Facilitate completion when using kill.
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# The following lines were added by compinstall.
# They have to do with completion and when to show a menu for selection.
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=4
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' squeeze-slashes true
autoload -Uz compinit; compinit


## Prompt ##
autoload -Uz promptinit
promptinit; prompt pure
