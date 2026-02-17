# Entorno común (login + no-login) para zsh.
# Mantén aquí variables y PATH. Evita cosas interactivas (prompt, bindkeys).

# Locale
export LANG=${LANG:-en_US.UTF-8}

# Homebrew (si existe)
if command -v brew >/dev/null 2>&1; then
  eval "$(brew shellenv)"
elif [ -x /opt/homebrew/bin/brew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Añadir a PATH de forma idempotente (equivalente a fish_add_path)
path_add() {
  local dir="$1"
  [ -n "$dir" ] || return 0
  [ -d "$dir" ] || return 0
  case ":$PATH:" in
    *":$dir:"*) ;;
    *) PATH="$dir:$PATH" ;;
  esac
}

# PATHs extra que tienes en fish
path_add "$HOME/go/bin"
path_add "/Users/andres/.codeium/windsurf/bin"

export PATH

