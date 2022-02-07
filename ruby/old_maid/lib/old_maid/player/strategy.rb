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
      class AlwaysHead
        include Singleton

        def pick_from(_)
          0
        end
      end
      class AlwaysLast
        include Singleton

        def pick_from(drawn)
          drawn.providable_range.max
        end
      end
      class AlwaysCenter
        include Singleton

        def pick_from(drawn)
          min, *_, max = drawn.providable_range.to_a

          case [min, max]
          in [Integer, Integer]
            (min + max) / 2
          else
            0
          end
        end
      end
    end
  end
end
