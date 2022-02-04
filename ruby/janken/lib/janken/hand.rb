# frozen_string_literal: true

module Janken
  class Hand
    class << self
      def stone
        @stone ||= new(key: :stone, stronger_against: :scissors, index: 1)
      end
      def scissors
        @scissors ||= new(key: :scissors, stronger_against: :paper, index: 2)
      end
      def paper
        @paper ||= new(key: :paper, stronger_against: :stone, index: 3)
      end
    end

    private def initialize(
      key:,
      stronger_against:,
      index:
    )
      @key = key
      @stronger_against = stronger_against
      @index = index
    end

    def <=>(other)
      index <=> other.index
    end

    # @return [Integer]
    protected def index
      @index
    end

    class List
      class EmptyHandListError < StandardError; end

      def self.at_least_one(hands)
        raise EmptyHandListError.new("at least on hand exist") if hands.empty?

        new(hands)
      end

      private def initialize(hands)
        @hands = hands
      end

      def winner
        case kinds.sort
        when [Janken::Hand.stone, Janken::Hand.scissors].sort
          Janken::Hand.stone
        when [Janken::Hand.stone, Janken::Hand.paper].sort
          Janken::Hand.paper
        else
          Janken::Hand.scissors
        end
      end

      def maybe_winner
        return ::Utils::Maybe.none if include_all? || include_only_one?

        ::Utils::Maybe.some case kinds.sort
                            when [Janken::Hand.stone, Janken::Hand.scissors].sort
                              Janken::Hand.stone
                            when [Janken::Hand.stone, Janken::Hand.paper].sort
                              Janken::Hand.paper
                            else
                              Janken::Hand.scissors
                            end
      end

      private

      def include_all?
        kinds.length == 3
      end

      def include_only_one?
        kinds.length == 1
      end

      def kinds
        @kinds ||= @hands.uniq
      end
    end
  end
end
