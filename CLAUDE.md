# CLAUDE.md

## Repository Overview

Personal dotfiles repository. Manages development environment configuration through symlinked files using a Makefile-based installation system.

## Key Commands

- `make install` - Install dotfiles as symlinks to $HOME directory
- `make brew` - Update homebrew packages using Brewfile
- `make keyrepeat` - Set optimal key repeat settings for macOS
- `make check-deadlinks` - Check for broken symlinks pointing to this repository
- `make help` - Show all available commands

## Architecture

### File Installation System
- Uses GNU Make to create symlinks from repository files to home directory
- Sources determined by `git ls-files` excluding `.install-ignore` patterns
- Target paths mirror repository structure under `$HOME`
- Creates necessary parent directories with proper permissions

### Configuration Structure
- `.config/` - XDG-style configuration files for various tools
- `.local/bin/` - Custom utility scripts (automatically added to PATH)
- `Brewfile` - Homebrew package definitions
- Root level dotfiles (like `.zshrc`, `.gitconfig`) symlinked directly

### Brewfile Management Rules
- All `brew` and `cask` entries must be sorted alphabetically within their groups (brew, cask, font)
- Inline comments are acceptable for packages that need explanation
- Maintain consistent formatting with single quotes around package names

## Common Patterns & Anti-Patterns

### Preferred Patterns
- Use Makefile targets for orchestration (not shell scripts)
- Symlink approach for dotfiles (never copy files)
- XDG-style .config directory structure when applicable

### Anti-Patterns to Avoid
- Direct file copies instead of symlinks
- Mixed package managers (avoid npm global installs, pip --user, etc.)
- Unsorted Brewfile entries
- Creating dotfiles outside tracked directories
- Hardcoded absolute paths (use $HOME or ~ variables)

## Review Criteria

When reviewing changes to this repository, verify:
- Brewfile entries are alphabetically sorted within their groups
- New configuration files follow XDG standards (.config/ when applicable)
- Make targets are documented in help output
- Custom scripts are executable and added to .local/bin/
- Symlink paths correctly mirror repository structure to $HOME
- No hardcoded absolute paths (use $HOME variable)

## Key Project Files

- `Makefile` - Primary installation and maintenance orchestration
- `Brewfile` - Homebrew package and application definitions
- `.editorconfig` - Format consistency rules for all file types
- `.install-ignore` - Files excluded from symlink installation
- `.config/` - XDG Base Directory compliant configurations
- `.local/bin/` - Custom utility scripts (automatically added to PATH)
- `.zshrc` - Shell configuration and environment setup
- `.config/git/config` - Git settings and custom command aliases
