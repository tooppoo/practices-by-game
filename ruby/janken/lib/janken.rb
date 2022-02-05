# frozen_string_literal: true

module Janken
  VERSION = '1.0.0'
end

require_relative './janken/game'
require_relative './janken/hand'
require_relative './janken/player'
require_relative './janken/strategy'

require_relative './utils'
