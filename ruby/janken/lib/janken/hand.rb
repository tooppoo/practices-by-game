# frozen_string_literal: true

require 'singleton'

module Janken
  module Hand
    class << self
      def stone
        @stone ||= Kind::Stone.instance
      end
      def scissors
        @scissors ||= Kind::Scissors.instance
      end
      def paper
        @paper ||= Kind::Paper.instance
      end
    end

    module Kind
      module EqBySym
        def ==(other)
          to_sym == other.to_sym
        end
        def eql?(other)
          self == other
        end
        def hash
          to_sym.hash
        end
      end
      class Stone
        include EqBySym, Singleton

        def stronger_than(other)
          other == Hand.scissors
        end

        def to_sym
          :stone
        end
      end
      class Scissors
        include EqBySym, Singleton

        def stronger_than(other)
          other == Hand.paper
        end

        def to_sym
          :scissors
        end
      end
      class Paper
        include EqBySym, Singleton

        def stronger_than(other)
          other == Hand.stone
        end

        def to_sym
          :paper
        end
      end
    end

    class List
      class EmptyHandListError < StandardError; end

      def self.at_least_one(hands)
        raise EmptyHandListError.new("at least on hand exist") if hands.empty?

        new(hands)
      end

      private attr_reader :hands

      private def initialize(hands)
        @hands = hands
      end

      def maybe_winner
        return ::Utils::Maybe.none if include_all? || include_only_one?

        a, b = kinds

        ::Utils::Maybe.some(a.stronger_than(b) ? a : b)
      end

      private

      def include_all?
        kinds.length == 3
      end

      def include_only_one?
        kinds.length == 1
      end

      def kinds
        @kinds ||= hands.uniq
      end
    end
  end
end
