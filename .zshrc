#Set up the prompt

autoload -Uz promptinit
promptinit
prompt adam1

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Use vi keybindings
# bindkey -v

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# colored ls and grep
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias rgrep='rgrep --color=auto'

if [ -n "$TMUX" ]; then
    function less_tmux()
    {
        eval server=\${$#}
        if [ -t 0 ]; then
            tmux split-window -p 65 "exec /usr/bin/less $@"
        else
            cat > /tmp/tmux.tmp && tmux split-window -p 65 "less /tmp/tmux.tmp"
        fi
    }

    compdef _less less_tmux

    function vi_tmux()
    {
        eval server=\${$#}
        tmux split-window -p 65 "exec /usr/bin/vim $@"
    }

    compdef _vim vi_tmux

    function man_tmux()
    {
        eval server=\${$#}
        tmux split-window -p 65 "exec /usr/bin/man $@"
    }

    compdef _man man_tmux

    alias less='less_tmux'
    alias less_std='/usr/bin/less'
    alias vi='vi_tmux'
    alias vi_std='/usr/bin/vim'
    alias vim='vi_tmux'
    alias vim_std='/usr/bin/vim'
    alias man='man_tmux'
    alias man_std='/usr/bin/man'
fi
