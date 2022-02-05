# frozen_string_literal: true

require_relative './hand'

module Janken
  class Game
    private attr_reader :times, :players

    def initialize(times:, players:)
      raise ArgumentError.new("players must exist at least 2") if players.length < 2

      @times = times

      @players = players
    end

    def play
      (1..times).each do
        selected_hands = players.map(&:select_hand)

        selected_hand_list = Janken::Hand::List.at_least_one(selected_hands.map(&:hand))
        selected_hand_list.maybe_winner.map do |hand_of_winner|
          selected_hands
            .select { |h| h == hand_of_winner }
            .each { |h| h.owner.notify_win }
        end
      end

      Result::Builder.new(players: players).build
    end

    module Result
      class Builder
        private attr_reader :players

        def initialize(players:)
          @players = players

          @max_win_count = Janken::Player::WinCount.new(1)
          @winners = []
          @losers = []
        end

        def build
          players.each do |player|
            if @max_win_count == player.win_count
              @winners << player
            elsif @max_win_count < player.win_count
              @max_win_count = player.win_count
              @losers = @winners
              @winners = [player]
            else
              @losers << player
            end
          end

          if @losers.length == players.length || @winners.length == players.length
            Draw.new
          else
            WinnerExist.new(@winners, @losers)
          end
        end
      end

      WinnerExist = Struct.new(:winners, :losers) do
        def draw?
          false
        end
      end

      class Draw
        def winners
          []
        end
        def losers
          []
        end
        def draw?
          true
        end
      end
    end
  end
end
