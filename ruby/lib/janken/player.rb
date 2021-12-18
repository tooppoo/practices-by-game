# frozen_string_literal: true

module Janken
  class Player
    attr_reader :win_count

    def initialize(name:, strategy:)
      @name = name
      @strategy = strategy

      @win_count = 0
    end

    def select_hand
      SelectedHand.new(@strategy.select, self)
    end

    def notify_win
      @win_count += 1

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
  end
end
