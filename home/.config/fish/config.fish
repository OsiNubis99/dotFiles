if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Brew
fish_add_path /opt/homebrew/bin

# Go
fish_add_path $HOME/go/bin

# zoxide
zoxide init --cmd cd fish | source

# Style
starship init fish | source
