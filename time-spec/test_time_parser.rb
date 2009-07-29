require 'time_parser_rb'

include TimeParser

p(parse_moment("tomorrow at 10", Time.new, Time.new))
p(parse_interval("every day from 14 for 1 hour", Time.new, Time.new))
p(parse_interval("always", Time.new, Time.new))
p(parse_interval("never", Time.new, Time.new))
