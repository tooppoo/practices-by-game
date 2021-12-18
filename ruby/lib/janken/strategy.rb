# frozen_string_literal: true

require_relative './hand'

module Janken
  class Strategy
    class Order
      def self.by(hands)
        new(hands)
      end

      def initialize(hands)
        @hands = hands
        @index = 0
      end

      def select
        @hands[@index % @hands.length].tap do
          @index += 1
        end
      end
    end

    class Only
      class << self
        def stone
          new(Hand.stone)
        end
        def scissors
          new(Hand.scissors)
        end
        def paper
          new(Hand.paper)
        end
      end

      private def initialize(only_hand)
        @only_hand = only_hand
      end

      def select
        @only_hand
      end
    end
  end
end
