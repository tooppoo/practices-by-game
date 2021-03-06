# frozen_string_literal: true

require 'rspec'
require 'rspec-parameterized'
require_relative '../../lib/old_maid'

RSpec.describe 'Card' do
  describe 'NumberCard' do
    context 'allowed numbers' do
      where(:card_number) do
        [
          [1],
          [5],
          [13],
        ]
      end

      with_them do
        it "should be allowed" do
          expect { OldMaid::Card::NumberCard.new(card_number) }.not_to raise_error
        end
      end
    end

    context 'disallowed numbers' do
      where(:case_name, :card_number) do
        [
          ['card number must be integer', 1.0],
          ['card number must be >= 1', 0],
          ['card number must be <= 13', 14],
        ]
      end

      with_them do
        it "should not be allowed" do
          expect { OldMaid::Card::NumberCard.new(card_number) }.to raise_error ArgumentError
        end
      end
    end

    describe 'compare' do
      where(:case_name, :card1, :card2, :expected) do
        [
          [
            'number 1 == number 1',
            OldMaid::Card::NumberCard.new(1),
            OldMaid::Card::NumberCard.new(1),
            true,
          ],
          [
            'number 1 == number 2',
            OldMaid::Card::NumberCard.new(1),
            OldMaid::Card::NumberCard.new(2),
            false,
          ],
          [
            'number 1 == joker',
            OldMaid::Card::NumberCard.new(1),
            OldMaid::Card::Joker.instance,
            false,
          ],
          [
            'joker == number 1',
            OldMaid::Card::Joker.instance,
            OldMaid::Card::NumberCard.new(1),
            false,
          ],
        ]
      end

      with_them do
        it "should return #{params[:expected]}" do
          expect(card1 == card2).to be expected
        end
      end
    end
  end
  describe 'Deck' do
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