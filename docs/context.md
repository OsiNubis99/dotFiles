# Contexto de arquitectura y convenciones

## Este repositorio

Este es el repositorio de dotFiles de Andres Hurtado (OsiNubis99).
Contiene todas las configuraciones del sistema macOS.

### Estructura

```
dotFiles/
├── home/          # Archivos de configuracion del sistema (symlinks)
├── docs/          # Documentacion e imagenes
├── CLAUDE.md      # Instrucciones para Claude Code
└── install.sh     # Script de instalacion
```

### Directorio `home/`

**IMPORTANTE**: Todo lo que esta en `home/` son symlinks a las configuraciones globales del sistema.
El script `install.sh` los enlaza a sus ubicaciones reales:

| Origen en repo              | Destino en sistema          |
|-----------------------------|-----------------------------|
| `home/.config/`             | `~/.config/`                |
| `home/.claude/`             | `~/.claude/`                |
| `home/.zshrc`               | `~/.zshrc`                  |
| `home/.zprofile`            | `~/.zprofile`               |
| `home/.ideavimrc`           | `~/.ideavimrc`              |
| `home/wallpapers/`          | `~/wallpapers/`             |

Al editar archivos en `home/`, estas modificando directamente las configs activas del sistema.

### Configs clave del sistema

- **Fish Shell**: `home/.config/fish/` - Shell principal
- **Zsh**: `home/.config/zsh/` + `home/.zshrc` - Shell alternativo
- **Neovim**: `home/.config/nvim/` - Editor (LazyVim)
- **Lazygit**: `home/.config/lazygit/` - Git UI
- **Starship**: `home/.config/starship.toml` - Prompt
- **Ghostty**: `home/.config/ghostty/` - Terminal principal
- **Karabiner**: `home/.config/karabiner/` - Remapeo de teclado
- **AeroSpace**: `home/.config/aerospace/` - Window tiling
- **Claude Code**: `home/.claude/` - Configuracion global de Claude

### Los dos `CLAUDE.md` del repo

Este repo contiene **dos** `CLAUDE.md` con propositos distintos. No confundirlos:

| Archivo | Symlink a | Rol | Alcance |
|---------|-----------|-----|---------|
| `CLAUDE.md` (raiz) | — (vive en el repo) | Instrucciones **del proyecto** dotFiles. Solo importa `@docs/context.md`. | Solo aplica cuando se trabaja EN este repo |
| `home/.claude/CLAUDE.md` | `~/.claude/CLAUDE.md` | Instrucciones **globales** de Claude Code. Las que Claude lee en CUALQUIER proyecto de la Mac. | Toda la maquina |

- El de la **raiz** es el `CLAUDE.md` estandar de cualquier repo (apunta a su `docs/context.md`).
- El de **`home/.claude/`** se enlaza con `stow` a `~/.claude/CLAUDE.md`, por eso Claude
  lo trata como el "global": preferencias generales, convenciones de comentarios,
  ClickUp, reglas de operaciones destructivas, y el puntero a [codeaver.server](../home/.claude/docs/codeaver-server.md).
- Editar `home/.claude/CLAUDE.md` en el repo modifica directamente el global activo
  (mismo inode via stow).

---

## Convenciones para todos los repos

Este esquema debe usarse en **todos** los proyectos. Al iniciar en cualquier repo:

1. **Siempre leer `docs/context.md`** antes de hacer cualquier cambio
2. **Verificar si existe `CLAUDE.md`** en el root del proyecto y leerlo
3. Si no existe `docs/context.md`, crear uno con la arquitectura del proyecto antes de empezar

### Estructura minima de cualquier repo

```
proyecto/
├── docs/
│   └── context.md   # Arquitectura, decisiones tecnicas, convenciones
├── CLAUDE.md        # Instrucciones para Claude Code (importa context.md)
└── ...
```

### Lo que debe documentar `docs/context.md` en cada repo

- Descripcion del proyecto y su proposito
- Stack tecnologico y versiones relevantes
- Estructura de directorios con explicacion
- Convenciones de codigo del proyecto
- Comandos importantes (dev, build, test, deploy)
- Decisiones de arquitectura tomadas

