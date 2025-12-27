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
- **Editor**: Emacs with extensive language support
- **Package Manager**: Homebrew preferred for development tools
- **Version Control**: Git with delta integration

## Code Style Standards

- Follow `.editorconfig` settings when available
- Use 2 spaces for indentation (except Makefiles which use tabs)
- Ensure UTF-8 encoding and LF line endings
- Always include final newline in files
- No unnecessary comments unless explicitly requested

## Tool Preferences

- **Search**: Use ripgrep (`rg`) over `grep`
- **File Operations**: Use `fd` over `find` when available
- **JSON Processing**: Use `jq` for JSON manipulation
- **GitHub**: Use `gh` command for all GitHub operations (issues, PRs, repos, etc.)

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
