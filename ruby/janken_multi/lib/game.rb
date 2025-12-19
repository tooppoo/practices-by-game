
require_relative './judge'

module JankenMulti
  class Game
    def initialize(players:)
      @players = players
      @judge = JankenMulti::Judge.new
    end

    def play_round!(times:, &block)
      times.times do
        input = JankenMulti::Judge::Input.new
        @players.each do |player|
          input.add(player: player, hand: player.show_hand)
        end

        res = @judge.results(input).when_not_draw do |result|
          result.winners.each do |winner|
            winner.notify_win!
          end

          result.losers.each do |loser|
            loser.notify_lose!
          end
        end

        if block_given?
          yield res
        end
      end
    end
  end
end
