# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository that manages development environment configuration through symlinked files. The repository uses a Makefile-based installation system to symlink configuration files from the repository to the home directory.

## Key Commands

- `make install` - Install dotfiles as symlinks to $HOME directory
- `make brew` - Update homebrew packages using Brewfile
- `make go` - Update Go development tools
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
- Go tools installed via `make go` target
- Development environment includes tools like ripgrep, fd, fzf, gh, etc.

### Editor Configuration
- Emacs configuration with extensive snippets for multiple languages
- Language-specific snippets for Perl, Ruby, Python, JavaScript, Terraform, etc.
- Git integration with tig, delta, and custom git commands

## Development Environment
- Shell: zsh with syntax highlighting and completions
- Editor: Emacs with language-specific configurations
- Terminal: iTerm2 with 24-bit color support
- Version control: Git with custom commands and delta integration
- Package managers: Homebrew, rbenv, plenv, tfenv