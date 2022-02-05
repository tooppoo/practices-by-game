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

      Result::Total.new(players)
    end

    module Result
      class Total
        attr_reader :winners

        def initialize(players)
          init = {
            most_win_count: Janken::Player::WinCount.zero,
            winners: [],
            losers: [],
          }

          sum = players.inject(init) do |xs, player|
            if not player.has_won?
              xs.merge({ losers: xs[:losers] + [player] })
            elsif xs[:most_win_count] == player.win_count
              xs.merge({ winners: xs[:winners] + [player] })
            elsif xs[:most_win_count] < player.win_count
              {
                most_win_count: player.win_count,
                winners: [player],
                losers: xs[:winners],
              }
            else
              xs.merge({ losers: xs[:losers] + [player] })
            end
          end

          @winners = sum[:winners]
          @losers = sum[:losers]
          @whole_players = players
        end

        def losers
          if @losers.length == @whole_players.length
            []
          else
            @losers
          end
        end

        def draw?
          winners.empty? && losers.empty?
        end

        private def whole_players(results)
          results.map(&:players).flatten.uniq
        end
      end
    end
  end
end
