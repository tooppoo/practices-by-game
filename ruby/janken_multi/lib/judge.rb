
require_relative 'hand'

module JankenMulti
  class Judge
    def results(input)
      hands = input.hands

      if hands.length == 1 || hands.length == 3
        Result.draw
      else
        case hands
        in [Hand::STONE, Hand::PAPER] | [Hand::PAPER, Hand::STONE]
          Result.new(winners: input.players_of(Hand::PAPER), losers: input.players_of(Hand::STONE))
        in [Hand::SCISSORS, Hand::PAPER] | [Hand::PAPER, Hand::SCISSORS]
          Result.new(winners: input.players_of(Hand::SCISSORS), losers: input.players_of(Hand::PAPER))
        in [Hand::STONE, Hand::SCISSORS] | [Hand::SCISSORS, Hand::STONE]
          Result.new(winners: input.players_of(Hand::STONE), losers: input.players_of(Hand::SCISSORS))
        end
      end
    end

    class Input
      def initialize
        @hand_players_map = {}
      end

      def add(player:, hand:)
        @hand_players_map[hand] ||= []
        @hand_players_map[hand] << player
      end

      def players_of(hand)
        @hand_players_map[hand] || []
      end
      
      def hands
        @hand_players_map.keys
      end
    end

    class Result
      def self.draw
        DrawResult.new
      end

      attr_reader :winners, :losers
      
      def initialize(winners:, losers:)
        @winners = winners
        @losers = losers
      end

      def ==(other)
        if other.is_a?(DrawResult)
          false
        else
          @winners == other.winners && @losers == other.losers
        end
      end

      def when_draw(&block)
        self
      end
      def when_not_draw(&block)
        yield self
        self
      end

      class DrawResult
        def ==(other)
          other.is_a?(DrawResult)
        end

        def when_draw(&block)
          yield self
          self
        end
        def when_not_draw(&block)
          self
        end
      end
    end
  end
end
