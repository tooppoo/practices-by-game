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
        name: Janken::Player::Name.new("Ulike Norman Owen"),
        strategy: Janken::Strategy::Order.by([Janken::Hand.paper, Janken::Hand.stone, Janken::Hand.scissors])
      )
    }
    let(:player2) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Una Nancy Owen"),
        strategy: Janken::Strategy::Only.stone
      )
    }
    let(:player3) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Agatha Christie"),
        strategy: Janken::Strategy::Only.scissors
      )
    }

    context 'play 3times' do
      let(:times) { 3 }

      it 'player2 is winner because most won' do
        result = game.play

        expect(result.winners).to match_array [player2]
      end
      it 'player1 & player3 are looser' do
        result = game.play

        expect(result.losers).to match_array [player1, player3]
      end
      it 'not draw game' do
        result = game.play
        
        expect(result.is_draw?).to eq false
      end
    end
  end

  context 'player1~3 exists, player1 & player2 show only stone, player3 show only scissors' do
    let(:player1) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Ulike Norman Owen"),
        strategy: Janken::Strategy::Only.stone
      )
    }
    let(:player2) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Una Nancy Owen"),
        strategy: Janken::Strategy::Only.stone
      )
    }
    let(:player3) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Agatha Christie"),
        strategy: Janken::Strategy::Only.scissors
      )
    }

    context 'play 3times' do
      let(:times) { 3 }

      it 'player1 & player2 are winner because most won' do
        result = game.play

        expect(result.winners).to match_array [player1, player2]
      end
      it 'player3 is looser' do
        result = game.play

        expect(result.losers).to match_array [player3]
      end
      it 'not draw game' do
        result = game.play

        expect(result.is_draw?).to eq false
      end
    end
  end

  context 'player1~3 exists, all players show only stone' do
    let(:player1) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Ulike Norman Owen"),
        strategy: Janken::Strategy::Only.stone
      )
    }
    let(:player2) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Una Nancy Owen"),
        strategy: Janken::Strategy::Only.stone
      )
    }
    let(:player3) {
      Janken::Player.new(
        name: Janken::Player::Name.new("Agatha Christie"),
        strategy: Janken::Strategy::Only.stone
      )
    }

    context 'play 3times' do
      let(:times) { 3 }

      it 'winners not exist' do
        result = game.play

        expect(result.winners).to match_array []
      end
      it 'losers not exist' do
        result = game.play

        expect(result.losers).to match_array []
      end
      it 'is draw game' do
        result = game.play

        expect(result.is_draw?).to eq true
      end
    end
  end
end
