guard :shell do
  watch(%r{.+\.erl}) {|m| `rake build && rake eunit` }
end
