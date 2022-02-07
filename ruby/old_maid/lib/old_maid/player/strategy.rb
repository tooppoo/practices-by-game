# frozen_string_literal: true

require 'singleton'

module OldMaid
  class Player
    module Strategy
      class DrawRandom
        include Singleton

        def pick_from(drawn)
          drawn.providable_range.to_a.sample
        end
      end
    end
  end
end
