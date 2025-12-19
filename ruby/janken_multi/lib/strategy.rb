
require_relative 'hand'

module JankenMulti
  module Strategy
    class RandomStrategy
      def next_hand
        [
          JankenMulti::Hand::STONE,
          JankenMulti::Hand::PAPER,
          JankenMulti::Hand::SCISSORS
        ].sample
      end
    end
    class OrderStrategy
      def initialize(hands)
        @hands = hands
        @index = 0
      end

      def next_hand
        hand = @hands[@index]
        @index = if @index + 1 >= @hands.length
          0
        else
          @index + 1
        end

        hand
      end
    end
  end
end
