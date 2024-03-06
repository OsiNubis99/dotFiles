if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -x PATH $PATH /home/andres/.ebcli-virtual-env/executables

zoxide init --cmd cd fish | source
starship init fish | source
