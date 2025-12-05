# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository that manages development environment configuration through symlinked files. The repository uses a Makefile-based installation system to symlink configuration files from the repository to the home directory.

## Common Patterns & Anti-Patterns

### Preferred Patterns
- Use Makefile targets for orchestration (not shell scripts)
- Symlink approach for dotfiles (never copy files)
- XDG-style .config directory structure when applicable
- Homebrew for all package management
- zsh built-in features over external commands when possible

### Anti-Patterns to Avoid
- Direct file copies instead of symlinks
- Mixed package managers (avoid npm global installs, pip --user, etc.)
- Unsorted Brewfile entries (must maintain alphabetical order within groups)
- Creating dotfiles outside tracked directories
- Hardcoded absolute paths (use $HOME or ~ variables)
- Using GNU-specific command flags on macOS BSD commands

## Key Commands

- `make install` - Install dotfiles as symlinks to $HOME directory
- `make brew` - Update homebrew packages using Brewfile
- `make terminfo` - Install 24-bit color terminal support
- `make keyrepeat` - Set optimal key repeat settings for macOS
- `make rainbow` - Test terminal 24-bit color support
- `make help` - Show all available commands

## Architecture

### File Installation System
- Uses GNU Make to create symlinks from repository files to home directory
- Sources determined by `git ls-files` excluding `.install-ignore` patterns
- Target paths mirror repository structure under `$HOME`
- Creates necessary parent directories with proper permissions

### Configuration Structure
- `.config/` - XDG-style configuration files for various tools
- `bin/` - Custom utility scripts added to PATH
- `Brewfile` - Homebrew package definitions
- Root level dotfiles (like `.zshrc`, `.gitconfig`) symlinked directly

### Package Management
- Homebrew packages managed via `Brewfile` with both brews and casks
- Development environment includes tools like ripgrep, fd, fzf, gh, etc.

#### Brewfile Management Rules
- **Alphabetical sorting**: All `brew` and `cask` entries must be sorted alphabetically within their groups (brew, cask, font)
- **Prefer Homebrew**: Use Homebrew over other package managers (npm, pip, etc.) for development tools when available
- **Comments allowed**: Inline comments are acceptable for packages that need explanation (e.g., build dependencies)
- **Consistency**: Maintain consistent formatting with single quotes around package names

### Editor Configuration
- Emacs with configuration at `.config/emacs/init.el`
- Extensive snippets for multiple languages and tree-sitter mode support
- Language-specific snippets for Perl, Ruby, TypeScript, JavaScript, Terraform, etc.
- Git integration with tig, delta, and custom git commands

## Review Criteria

When reviewing changes to this repository, verify:
- Brewfile entries are alphabetically sorted within their groups (brew, cask, font)
- New configuration files follow XDG standards (.config/ when applicable)
- Make targets are documented in help output
- Custom scripts are executable and added to bin/
- Symlink paths correctly mirror repository structure to $HOME
- No hardcoded absolute paths (use $HOME variable)
- Shell scripts use zsh built-ins or BSD-compatible commands
- Changes maintain compatibility with macOS environment

## Development Environment
- Shell: zsh with syntax highlighting and completions
- Editor: Emacs with language-specific configurations
- Terminal: Ghostty with 24-bit color support
- Version control: Git with custom commands and delta integration
- Package managers: Homebrew, rbenv, plenv, tfenv
- Build system: GNU Make

## Known Limitations & Workarounds

### macOS Platform Specifics
- Target platform: macOS with BSD commands (not GNU)
- BSD sed/awk syntax differs from GNU versions (use gsed/gawk from coreutils when needed)
- BSD find command has different options than GNU find
- BSD xargs requires -0 flag for null-terminated input
- Use Homebrew-installed GNU tools when needed (prefixed with 'g': gls, gdircolors, gsed)
- Prefer zsh built-in features over external commands for compatibility
- bc calculator not available by default - use zsh arithmetic where possible

### Post-Installation Actions
- Shell completions require new shell session to take effect
- Ghostty keybindings require terminal restart to apply
- Some Emacs packages may require manual M-x package-install on first run
- Key repeat settings (make keyrepeat) require system reboot to fully take effect

### Symlink Conflicts
- Backup existing dotfiles before `make install` if conflicts exist
- Use `ls -la ~` to identify existing dotfiles before installation
- Conflicting files must be manually removed or backed up before installation

## Troubleshooting

### Symlink Installation Issues
- **Error: File exists**: Backup or remove existing file before running `make install`
  - Solution: `mv ~/.existing-file ~/.existing-file.backup`
- **Permission denied**: Check parent directory permissions
  - Solution: Ensure home directory and subdirectories are writable
- **Broken symlinks**: Remove and reinstall
  - Check: `find ~ -maxdepth 1 -type l ! -exec test -e {} \; -print`
  - Fix: `make install` to recreate correct symlinks

### Shell Completions Not Working
- **Symptom**: Tab completion doesn't work for commands
  - Clear completion cache: `rm -f ~/.cache/zsh/zcompdump*`
  - Start new shell session or run: `exec zsh`
  - Verify zsh-completions in FPATH: `echo $FPATH`

### Homebrew Package Issues
- **Packages not found or outdated**
  - Run `brew doctor` to identify issues
  - Update: `brew update && brew upgrade`
  - Reinstall: `brew reinstall <package>`
- **Brewfile sync fails**
  - Verify Brewfile syntax is valid Ruby
  - Check alphabetical sorting within each group (brew, cask, font)

### Emacs Configuration Issues
- **Packages not loading**
  - Run `M-x package-refresh-contents` in Emacs
  - Check `*Messages*` buffer for errors
  - Verify package archives are accessible
- **Performance issues**
  - Check for large recentf: `wc -l ~/.config/emacs/recentf`
  - Clear old entries: Edit recentf or increase recentf-max-saved-items

### Terminal Display Issues
- **Colors not displaying correctly**
  - Verify 24-bit color support: `make rainbow`
  - Ensure TERM is set correctly: `echo $TERM`
  - Reinstall terminfo: `make terminfo`
- **Ghostty keybindings not working**
  - Restart Ghostty completely (Cmd+Q, not just close window)
  - Verify config: `cat ~/.config/ghostty/config`

## Key Project Files

- `Makefile` - Primary installation and maintenance orchestration
- `Brewfile` - Homebrew package and application definitions
- `.editorconfig` - Format consistency rules for all file types
- `.install-ignore` - Files excluded from symlink installation
- `.config/` - XDG Base Directory compliant configurations
- `bin/` - Custom utility scripts (automatically added to PATH)
- `.zshrc` - Shell configuration and environment setup
- `.gitconfig` - Git settings and custom command aliases

## Code Style
- Follow `.editorconfig` settings for consistent formatting across all files
- Use 2 spaces for indentation (except Makefiles which use tabs)
- Ensure UTF-8 encoding and LF line endings
- Always include final newline in files
