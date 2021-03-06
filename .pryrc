# -*- mode: ruby -*-
if defined?(PryByebug)
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 'f', 'finish'
end

if defined?(Rails::Console)
  ActiveRecord::Base.logger = Logger.new(STDOUT)
end

Pry.config.editor = proc { |file, line| "emacsclient -n +#{line} #{file}" }
