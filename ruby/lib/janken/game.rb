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

          selected_hand_list = Janken::Hand::List.at_least_one(selected_hands.map(&:hand))
          selected_hand_list.maybe_winner.map do |hand_of_winner|
            @selected_hands.select { |h| h == hand_of_winner }
                           .each { |h| h.owner.notify_win }
          end
        end

        def players
          @selected_hands.map(&:owner).uniq
        end
      end

      class Total
        attr_reader :winners

        def initialize(each_results)
          @each_results = each_results

          init = { most_win_count: Janken::Player::WinCount.zero, winners: [] }
          sum = whole_players.inject(init) do |xs, player|
            if not player.has_won?
              xs
            elsif xs[:most_win_count] == player.win_count
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
