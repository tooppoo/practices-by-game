# frozen_string_literal: true

require 'rspec'
require_relative '../../lib/old_maid'

RSpec.describe 'Dealer' do
  let(:deck) {
    OldMaid::Card::Deck.new_pack.tap do |d|
      # シャッフル固定
      allow(d).to receive(:shuffle).and_return(d)
    end
  }

  describe 'deal cards to players from a deck' do
    context '5 players exist' do
      it 'should deal cards to each player' do
        prepared_players = (1..5).map do |index|
          OldMaid::Player.prepare(name: "player-#{index}")
        end

        sut = OldMaid::Dealer.new(deck)

        n = ->(num) { OldMaid::Card::NumberCard.new(num) }
        joker = OldMaid::Card::Joker.instance
        player = ->(i, cards) {
          pl = cards.inject(prepared_players[i]) do |pl, c|
            pl.accept c
          end

          pl.get_ready
        }

        expected = [
          player.call(0, [n[1], n[6],  n[11], n[3],  n[8], n[13], n[5], n[10], n[2],  n[7], n[12]]),
          player.call(1, [n[2], n[7],  n[12], n[4],  n[9],  n[1], n[6], n[11], n[3],  n[8], n[13]]),
          player.call(2, [n[3], n[8],  n[13], n[5], n[10],  n[2], n[7], n[12], n[4],  n[9], joker]),
          player.call(3, [n[4], n[9],   n[1], n[6], n[11],  n[3], n[8], n[13], n[5], n[10]       ]),
          player.call(4, [n[5], n[10],  n[2], n[7], n[12],  n[4], n[9],  n[1], n[6], n[11]       ]),
        ]

        expect(sut.deal_to(prepared_players)).to eq expected
      end
    end
  end
end