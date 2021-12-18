# frozen_string_literal: true

module Janken
  class Player
    attr_reader :win_count

    def initialize(name:, strategy:)
      @name = name
      @strategy = strategy

      @win_count = Player::WinCount.zero
    end

    def select_hand
      SelectedHand.new(@strategy.select, self)
    end

    def has_won?
      not @win_count.is_zero?
    end

    def notify_win
      @win_count = @win_count.increment

      self
    end

    def ==(other)
      name == other.name
    end

    protected def name
      @name
    end

    class SelectedHand
      attr_reader :hand, :owner

      def initialize(hand, owner)
        @hand = hand
        @owner = owner
      end

      def ==(hand)
        case hand
        when Janken::Hand
          @hand == hand
        else
          self == hand
        end
      end
    end

    class WinCount
      class InvalidCountNumber < StandardError; end

      def self.zero
        @zero ||= new(0)
      end

      def initialize(value)
        raise InvalidCountNumber.new("win count must be >= 0, but actual is #{value}") unless value >= 0
        raise InvalidCountNumber.new("win count must be integer, but actual is #{value}") unless value.is_a? Integer

        @value = value
      end

      def ==(other)
        value == other.value
      end
      def <(other)
        value < other.value
      end
      def >(other)
        value > other.value
      end

      def increment
        WinCount.new(value + 1)
      end

      def is_zero?
        value == 0
      end

      def to_i
        value
      end

      protected def value
        @value
      end
    end
  end
end
