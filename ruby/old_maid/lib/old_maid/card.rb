# frozen_string_literal: true

module OldMaid
  module Card
    class NumberCard
      attr_reader :number

      def initialize(number)
        raise ArgumentError.new("card number must be integer") unless number.integer?
        raise ArgumentError.new("card number must be 1 <= x <= 13") unless (1..13).include?(number)

        @number = number
      end

      def ==(other)
        case other
        when Joker
          false
        else
          number == other.number
        end
      end

      def to_sym
        number.to_s.to_sym
      end
    end
    class Joker
      require 'singleton'

      include Singleton

      def to_sym
        :joker
      end
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

      def empty?
        size == 0
      end

      def to_a
        cards.dup.freeze
      end

      def shuffle
        Deck.new(cards.shuffle)
      end

      TakeCardTuple = Struct.new(:card, :rest)
      def take_one
        card, *rest = cards

        TakeCardTuple.new(card, Deck.new(rest))
      end

      def ==(other)
        cards == other.to_a
      end
    end
  end
end
