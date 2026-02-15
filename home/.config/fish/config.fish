if status is-interactive
    # Commands to run in interactive sessions can go here
end

export LANG=en_US.UTF-8

# Brew
fish_add_path /opt/homebrew/bin

# Go
fish_add_path $HOME/go/bin

# zoxide
zoxide init --cmd cd fish | source

# Style
starship init fish | source

# Added by Windsurf
fish_add_path /Users/andres/.codeium/windsurf/bin
