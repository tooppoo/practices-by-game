# frozen_string_literal: true

require 'rspec'
require_relative '../../lib/old_maid'

RSpec.describe 'Card' do
  RSpec.describe 'Deck' do
    context 'open new pack' do
      it 'should contains 53 cards' do
        sut = OldMaid::Card::Deck.new_pack

        expect(sut.size).to eq 53
      end

      it 'should contains 1-13 * 4 + one Joker' do
        sut = OldMaid::Card::Deck.new_pack

        whole_cards = (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
        + (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
        + (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
        + (Range.new(1, 13).map { |n| OldMaid::Card::NumberCard.new(n) }) \
        + [OldMaid::Card::Joker.instance]

        expect(sut.to_a).to eq whole_cards
      end
    end

    context 'shuffle' do
      it 'should provide another deck object' do
        d1 = OldMaid::Card::Deck.new_pack
        d2 = OldMaid::Card::Deck.new_pack

        expect(d1.shuffle).not_to eq d2
      end
    end

    context 'take a card' do
      it 'should provide a card' do
        deck = OldMaid::Card::Deck.new_pack
        sut = deck.take_one

        expect(sut.card).to eq OldMaid::Card::NumberCard.new(1)
      end

      it 'should provide also rest cards as deck' do
        deck = OldMaid::Card::Deck.new_pack
        sut = deck.take_one

        expect(sut.rest.size).to eq 52
      end
    end
  end
end