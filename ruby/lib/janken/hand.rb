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
  end
end
