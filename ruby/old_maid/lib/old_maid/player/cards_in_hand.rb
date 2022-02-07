# frozen_string_literal: true

module OldMaid
  class Player
    class CardsInHand
      def self.empty
        new({})
      end

      protected attr_reader :cards

      protected def initialize(cards)
        @cards = cards
      end

      def ==(other)
        cards == other.cards
      end

      def sample
        cards.values.sample
      end
      def length
        cards.length
      end
      def empty?
        cards.empty?
      end

      def to_a
        cards.values.map(&:to_s)
      end

      def dump(card)
        with cards.reject { |_, c| c == card }
      end

      def include?(card)
        cards.include?(card.to_sym)
      end

      def add(card)
        with cards.merge({ card.to_sym => card })
      end

      private def with(cards)
        CardsInHand.new(cards)
      end
    end
  end
end
