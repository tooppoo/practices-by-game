# frozen_string_literal: true

module OldMaid
  require_relative './old_maid/card'
  require_relative './old_maid/dealer'
  require_relative './old_maid/game'
  require_relative './old_maid/player'

  module Util
    require_relative './util/event_emitter'
    require_relative './util/tuple'
  end
end
