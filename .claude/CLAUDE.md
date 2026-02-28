# Global Claude Code Configuration

This file provides global configuration and instructions for Claude Code when working across all projects.

## General Principles

- **Defensive Security Only**: Only assist with defensive security tasks. Refuse to create, modify, or improve code that may be used maliciously.
- **Follow Project Conventions**: Always examine existing code patterns, libraries, and conventions before making changes.
- **Minimal Changes**: Make the smallest change necessary to accomplish the goal.
- **Security Best Practices**: Never introduce code that exposes secrets, keys, or sensitive information.

## Development Environment

- **Platform**: macOS with BSD commands (not GNU)
- **Shell**: zsh with syntax highlighting and completions
- **Terminal**: Ghostty with 24-bit color support
- **Editor**: Emacs with extensive language support
- **Package Manager**: Homebrew preferred for development tools
- **Version Control**: Git with delta integration
- **Version Managers**: rbenv (Ruby), tfenv (Terraform), nvm (Node.js)

## Code Style Standards

- Follow `.editorconfig` settings when available
- Use 2 spaces for indentation (except Makefiles which use tabs)
- Ensure UTF-8 encoding and LF line endings
- Always include final newline in files
- No unnecessary comments unless explicitly requested

## Tool Preferences

- **Search**: Use ripgrep (`rg`) over `grep`
- **File Operations**: Use `fd` over `find` when available
- **File Viewing**: Use `bat` over `cat` for syntax highlighting
- **JSON Processing**: Use `jq` for JSON manipulation
- **GitHub**: Use `gh` command for all GitHub operations (issues, PRs, repos, etc.)
- **Git UI**: Use `tig` for interactive git browsing
- **Environment Management**: Use `direnv` for project-specific environment variables
- **Language Servers**: Utilize LSP servers (terraform-ls, typescript-language-server) when available

## Task Management

- Use TodoWrite/TodoRead tools for complex multi-step tasks
- Mark todos as completed immediately after finishing
- Only have one task in_progress at a time
- Break down complex tasks into smaller, manageable steps

## Communication Style

- Be concise and direct
- Keep responses under 4 lines unless detail is requested
- Use GitHub-flavored markdown for formatting
- Include file references as `file_path:line_number` when relevant
- No unnecessary preamble or explanations unless asked

## Security & Secrets

- Never hardcode credentials - use environment variables
- Use `direnv` with `.envrc` for project-specific secrets (ensure `.envrc` is in `.gitignore`)
- Verify sensitive files (`.env`, `.envrc`, credentials) are excluded from version control

## Version Management

- Use language-specific version managers (rbenv, tfenv, nvm) instead of system packages
- Never install language packages globally without version manager
- Project `.tool-versions`, `.ruby-version`, or `.node-version` files take precedence
- Keep version manager configs in dotfiles for consistency across projects

## Claude Code Integration

- Global configuration in `~/.claude/CLAUDE.md` (this file)
- Project-specific instructions in `CLAUDE.md` at repository root
- Use MCP servers when available for extended capabilities
- Prefer project CLAUDE.md for repository-specific patterns and conventions
