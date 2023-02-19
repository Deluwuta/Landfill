# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt beep notify

bindkey -e # -v: vi mode, -e: emacs mode
# End of lines configured by zsh-newuser-install

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# The following lines were added by compinstall
zstyle :compinstall filename '/home/delta/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
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
      *.tar.zst)   tar xf $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Im mentally ill
alias vhypr='nvim $HOME/.config/hypr/hyprland.conf'

# ls --colors = true
alias l='lsd -a --group-dirs=first --color=always' # Files + dirs
alias ls='lsd -a --group-dirs=first --color=always' # Files + dirs
alias la='lsd -alh --group-dirs=first --color=always' # Long
alias ll='lsd -l --group-dirs=first --color=always' # Long w/o dots
# alias lt='lsd --tree --group-dirs=first --color=always'

# Cat but better... sorta
alias cat='bat'

alias pacsyu='sudo pacman -Syu'
alias pacsyyu='sudo pacman -Syyu'

#Cleanup orphaned packages
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

alias df='df -h'

alias rm='rm -i'

alias shn='$HOME/russianRoulette.sh'

# PLUGINS SECTION #
source /usr/share/zsh/share/antigen.zsh

antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply
# END OF PLUGINS #

## Paths
PATH="$HOME/.emacs.d/bin:$PATH"
PATH="$HOME/eww/target/release:$PATH"

eval "$(starship init zsh)"
