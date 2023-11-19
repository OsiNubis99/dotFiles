export TERM="xterm-256color"
export LANG="en_US.UTF-8"
export HISTORY_IGNORE="(clear|ls.*|cd|pwd|exit|sudo reboot|history.*)"
# PATH
# Local
export PATH=$PATH:"/home/${MY_USER}/.local/bin"
# EB
export PATH="/home/andres/.ebcli-virtual-env/executables:$PATH"
# Ruby
export PATH="/home/andres/.rbenv/shims/:$PATH"
