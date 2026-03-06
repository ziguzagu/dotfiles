# Global Claude Code Configuration

## Development Environment

- **Platform**: macOS with BSD commands (not GNU)
- **Shell**: zsh with syntax highlighting and completions
- **Terminal**: Ghostty with 24-bit color support
- **Editor**: Emacs with extensive language support
- **Package Manager**: Homebrew preferred for development tools
- **Version Control**: Git with delta integration
- **Version Managers**: rbenv (Ruby), tfenv (Terraform), nvm (Node.js)
  - Use version managers instead of system packages
  - Project `.tool-versions`, `.ruby-version`, `.node-version` files take precedence

## Code Style Standards

- Follow `.editorconfig` settings when available
- Use 2 spaces for indentation (except Makefiles which use tabs)
- Ensure UTF-8 encoding and LF line endings
- Always include final newline in files
- No unnecessary comments unless explicitly requested

## Tool Preferences

- **Search**: Use `rg` over `grep`
- **File Operations**: Use `fd` over `find`
- **JSON Processing**: Use `jq` for JSON manipulation
- **GitHub**: Use `gh` command for all GitHub operations (issues, PRs, repos, etc.)
- **Environment Management**: Use `direnv` for project-specific environment variables and secrets (ensure `.envrc` is in `.gitignore`)

## Communication Style

- Keep responses under 4 lines unless detail is requested
- Include file references as `file_path:line_number` when relevant
