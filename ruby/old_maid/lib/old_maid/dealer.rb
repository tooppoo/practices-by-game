# frozen_string_literal: true

module OldMaid
  class Dealer
    private attr_reader :deck

    def initialize(deck)
      @deck = deck
    end

    PlayerInDealing = Struct.new(:index, :player) do
      def accept(card)
        PlayerInDealing.new(index, player.accept(card))
      end

      def <=>(other)
        index.<=>(other.index)
      end
    end
    def deal_to(players)
      shuffled = deck.shuffle

      players_in_dealing = players.map.with_index { |pl, i| PlayerInDealing.new(i, pl) }
      result = __deal_to(shuffled, players_in_dealing)

      result.sort.map { |player_dealing| player_dealing.player.get_ready }
    end

    private def __deal_to(deck, players_in_dealing)
      if deck.empty?
        players_in_dealing
      else
        res = deck.take_one
        card, deck_rest = res.card, res.rest

        current, *other_players = players_in_dealing

        __deal_to(deck_rest, other_players + [current.accept(card)])
      end
    end
  end
end
