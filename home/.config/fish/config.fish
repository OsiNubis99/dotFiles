if status is-interactive
    # Commands to run in interactive sessions can go here
end

fish_add_path /opt/homebrew/bin
fish_add_path $HOME/go/bin

zoxide init --cmd cd fish | source
starship init fish | source
