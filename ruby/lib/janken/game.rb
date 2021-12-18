# frozen_string_literal: true

require_relative './hand'

module Janken
  class Game
    def initialize(times:)
      @times = times

      @players = []
    end

    def <<(player)
      @players << player
    end

    def play
      result_by_games = (1..@times).map do
        selected_hands = @players.map(&:select_hand)

        Result::Each.create_for(selected_hands)
      end

      Result::Total.new(result_by_games)
    end

    module Result
      class Each
        def self.create_for(selected_hands)
          new(selected_hands)
        end

        private def initialize(selected_hands)
          @selected_hands = selected_hands

          return if kinds_of_selected_hands.length != 2

          hand_of_winner = case kinds_of_selected_hands.sort
                           when [Janken::Hand.stone, Janken::Hand.scissors].sort
                             Janken::Hand.stone
                           when [Janken::Hand.stone, Janken::Hand.paper].sort
                             Janken::Hand.paper
                           else
                             Janken::Hand.scissors
                           end

          @selected_hands.select { |h| h == hand_of_winner }
                         .each { |h| h.owner.notify_win }
        end

        def players
          @selected_hands.map(&:owner).uniq
        end

        private def kinds_of_selected_hands
          @selected_hands.map(&:hand).uniq
        end
      end

      class Total
        attr_reader :winners

        def initialize(each_results)
          @each_results = each_results

          sum = whole_players.inject({ most_win_count: 0, winners: [] }) do |xs, player|
            if xs[:most_win_count] == player.win_count
              xs.merge({ winners: xs[:winners] + [player] })
            elsif xs[:most_win_count] < player.win_count
              { most_win_count: player.win_count, winners: [player] }
            else
              xs
            end
          end

          @winners = sum[:winners]
        end

        def losers
          @losers ||= if is_draw?
                        []
                      else
                        whole_players.select do |player|
                          not winners.include?(player)
                        end
                      end
        end

        def is_draw?
          winners.empty?
        end

        private def whole_players
          @whole_players ||= @each_results.map(&:players).flatten.uniq
        end
      end
    end
  end
end