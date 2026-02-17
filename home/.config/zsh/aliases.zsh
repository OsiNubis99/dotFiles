# Aliases/funciones para imitar fish/functions

# ls -> lsd --group-directories-first --all
if command -v lsd >/dev/null 2>&1; then
  ls() { lsd --group-directories-first --all "$@"; }
fi

# vi/vim -> clear && nvim
if command -v nvim >/dev/null 2>&1; then
  vi() { clear; nvim "$@"; }
  vim() { clear; nvim "$@"; }
fi

# rm -> rm -R (ojo: esto es peligroso; lo clono tal cual)
rm() { /bin/rm -R "$@"; }

# doom
if [ -x "$HOME/.config/emacs/bin/doom" ]; then
  doom() { "$HOME/.config/emacs/bin/doom" "$@"; }
fi

