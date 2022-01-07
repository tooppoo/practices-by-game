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

    def inspect
      [name, @win_count].inspect
    end

    protected def name
      @name
    end

    class Name
      class InvalidPlayerNameError < StandardError; end

      def initialize(value)
        raise InvalidPlayerNameError.new("player name must not be empty but actual is empty") if value.empty?

        @value = value
      end

      def ==(other)
        value == other.value
      end

      def inspect
        @value
      end

      protected def value
        @value
      end
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

      def inspect
        [@owner, @hand].inspect
      end
    end

    class WinCount
      class InvalidCountNumberError < StandardError; end

      def self.zero
        @zero ||= new(0)
      end

      def initialize(value)
        raise InvalidCountNumberError.new("win count must be >= 0, but actual is #{value}") unless value >= 0
        raise InvalidCountNumberError.new("win count must be integer, but actual is #{value}") unless value.is_a? Integer

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

      def inspect
        value.to_s
      end

      protected def value
        @value
      end
    end
  end
end
