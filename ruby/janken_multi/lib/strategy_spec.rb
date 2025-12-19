
require 'rspec'
require_relative './strategy'
require_relative './hand'

describe 'Strategy' do
  describe 'OrderStrategy' do
    it 'should return moves in order' do
      strategy = JankenMulti::Strategy::OrderStrategy.new([
        JankenMulti::Hand::STONE,
        JankenMulti::Hand::PAPER,
        JankenMulti::Hand::SCISSORS,
      ])
      expect(strategy.next_hand).to eq JankenMulti::Hand::STONE
      expect(strategy.next_hand).to eq JankenMulti::Hand::PAPER
      expect(strategy.next_hand).to eq JankenMulti::Hand::SCISSORS
      expect(strategy.next_hand).to eq JankenMulti::Hand::STONE
    end
  end
  describe 'RandomStrategy' do
    it 'should return random moves' do
      strategy = JankenMulti::Strategy::RandomStrategy.new
      hands = []
      10.times do
        hands << strategy.next_hand
      end
      expect(hands.uniq.length).to be > 1
    end
  end
end
