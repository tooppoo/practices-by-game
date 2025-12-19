
require 'rspec'
require_relative './hand'

describe 'JankenMulti::Hand' do
  describe 'Hand classes' do
    it 'Stone to_s should return "stone"' do
      expect(JankenMulti::Hand::STONE.to_s).to eq "stone"
    end

    it 'Paper to_s should return "paper"' do
      expect(JankenMulti::Hand::PAPER.to_s).to eq "paper"
    end

    it 'Scissors to_s should return "scissors"' do
      expect(JankenMulti::Hand::SCISSORS.to_s).to eq "scissors"
    end

    it 'Stone equality' do
      expect(JankenMulti::Hand::STONE).to eq JankenMulti::Hand::STONE
      expect(JankenMulti::Hand::STONE).not_to eq JankenMulti::Hand::PAPER
      expect(JankenMulti::Hand::STONE).not_to eq JankenMulti::Hand::SCISSORS
    end

    it 'Paper equality' do
      expect(JankenMulti::Hand::PAPER).to eq JankenMulti::Hand::PAPER
      expect(JankenMulti::Hand::PAPER).not_to eq JankenMulti::Hand::STONE
      expect(JankenMulti::Hand::PAPER).not_to eq JankenMulti::Hand::SCISSORS
    end

    it 'Scissors equality' do
      expect(JankenMulti::Hand::SCISSORS).to eq JankenMulti::Hand::SCISSORS
      expect(JankenMulti::Hand::SCISSORS).not_to eq JankenMulti::Hand::STONE
      expect(JankenMulti::Hand::SCISSORS).not_to eq JankenMulti::Hand::PAPER
    end
  end
end
