# frozen_string_literal: true

require_relative './dealer'
require_relative './card'

module OldMaid
  class Game
    private attr_reader :players

    def initialize(players:)
      @players = players
    end

    def play
      deck = OldMaid::Card::Deck.new_pack
      dealer = OldMaid::Dealer.new(deck)

      players_get_ready = dealer.deal_to(players.shuffle).reject { |p| p.finished? }
    end
  end
end
