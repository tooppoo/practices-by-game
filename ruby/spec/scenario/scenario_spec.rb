# frozen_string_literal: true

require 'rspec'
require_relative '../../lib/janken'

describe 'Janken Scenario' do
  let(:game) do
    game = Janken::Game.new(times: 3).tap do |g|
      g << player1
      g << player2
      g << player3
    end
  end

  context 'player1~3 exists, player1 show hand paper, stone, scissors, player2 show only stone, player3 show only scissors' do
    let(:player1) {
      Janken::Player.new(
        name: "Ulike Norman Owen",
        strategy: Janken::Strategy::Order.by([Janken::Hand.paper, Janken::Hand.stone, Janken::Hand.scissors])
      )
    }
    let(:player2) {
      Janken::Player.new(
        name: "Una Nancy Owen",
        strategy: Janken::Strategy::Only.stone
      )
    }
    let(:player3) {
      Janken::Player.new(
        name: "Agatha Christie",
        strategy: Janken::Strategy::Only.scissors
      )
    }

    context 'play 3times' do
      let(:times) { 3 }

      it 'player2 is winner because most won' do
        result = game.play

        expect(result.winners).to match_array [player2]
      end
      it 'player1 & player3 is looser' do
        result = game.play

        expect(result.losers).to match_array [player1, player3]
      end
      it 'not draw game' do
        result = game.play
        
        expect(result.is_draw?).to eq false
      end
    end
  end
end
