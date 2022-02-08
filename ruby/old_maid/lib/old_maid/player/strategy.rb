# frozen_string_literal: true

require 'singleton'

module OldMaid
  class Player
    module Strategy
      class SelectViaStdInput
        include Singleton

        def pick_from(drawn)
          candidates = drawn.providable_range.to_a

          while true
             puts "#{drawn.name} provide #{candidates.join(', ')}."
             puts "you should draw a card from #{drawn.name}. type number of candidate:"
             input = gets.chomp.to_i

             if candidates.include? input
               break input
             else
               puts "#{input} is not provided. retry select."
             end
           end
        end
      end
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
