## -*- mode: ruby -*-
IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:AUTO_INDENT_MODE] = false

if defined?(Reline)
  Reline::Face.config(:completion_dialog) do |conf|
    conf.define :default, foreground: :white, background: :black
    conf.define :enhanced, foreground: :black, background: :white
    conf.define :scrollbar, foreground: :white, background: :black
  end
end
