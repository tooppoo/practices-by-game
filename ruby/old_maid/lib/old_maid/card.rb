# frozen_string_literal: true

module OldMaid
  module Card
    class NumberCard
      protected attr_reader :number

      def initialize(number)
        raise ArgumentError.new("card number must be integer") unless number.integer?
        raise ArgumentError.new("card number must be 1 <= x <= 13") unless (1..13).include?(number)

        @number = number
      end

      def ==(other)
        number == other.number
      end
    end
    class Joker
      require 'singleton'

      include Singleton
    end

    class Deck
      def self.new_pack
        cards = (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
          + (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
          + (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
          + (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
          + [OldMaid::Card::Joker.instance]

        new(cards)
      end

      protected def initialize(cards)
        @cards = cards
      end

      private attr_reader :cards

      def size
        cards.length
      end

      def to_a
        cards
      end

      def shuffle
        Deck.new(cards.shuffle)
      end

      TakeCardTuple = Struct.new(:card, :rest)
      def take_one
        card, *rest = cards

        TakeCardTuple.new(card, rest)
      end

      def ==(other)
        cards == other.to_a
      end
    end
  end
end
