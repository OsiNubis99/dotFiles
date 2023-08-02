#     Options section
autoload -U compinit colors zcalc
compinit -d
colors
setopt correct
setopt extendedglob
setopt nocaseglob
setopt rcexpandparam
setopt nocheckjobs                                       
setopt numericglobsort                                   
setopt nobeep                                            
setopt appendhistory                                     
setopt histignorealldups
setopt HIST_IGNORE_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt autocd                                            
setopt inc_append_history                                
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"  
zstyle ':completion:*' rehash true                       
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ${ZDOTDIR}/cache
HISTFILE="/home/${MY_USER}/.zhistory"
HISTSIZE='64000'
SAVEHIST="${HISTSIZE}"
WORDCHARS=${WORDCHARS//\/[&.;]}
#      Keybindings
bindkey -e
bindkey '^[[7~' beginning-of-line                
bindkey '^[[H' beginning-of-line                 
bindkey '^[[8~' end-of-line                      
bindkey '^[[F' end-of-line                       
bindkey '^[[2~' overwrite-mode                   
bindkey '^[[3~' delete-char                      
bindkey '^[[C'  forward-char                     
bindkey '^[[D'  backward-char                    
bindkey '^[[5~' history-beginning-search-backward
bindkey '^[[6~' history-beginning-search-forward
bindkey '^[Oc' forward-word                      
bindkey '^[Od' backward-word                     
bindkey '^[[1;5D' backward-word                  
bindkey '^[[1;5C' forward-word                   
bindkey '^H' backward-kill-word                  
bindkey '^[[Z' undo                              
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
