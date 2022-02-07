# frozen_string_literal: true

require 'rspec'
require 'rspec-parameterized'
require 'old_maid'

MockDrawn = Struct.new(:providable_range)

RSpec.describe 'OldMaid::Player::Strategy' do
  describe 'AlwaysCenter' do
    where(:case_name, :drawn, :expected) do
      [
        [
          'count of cards in hand is 5(odd)',
          OldMaid::Player
            .prepare(name: 'test')
            .accept(OldMaid::Card::NumberCard.new(1))
            .accept(OldMaid::Card::NumberCard.new(2))
            .accept(OldMaid::Card::NumberCard.new(3))
            .accept(OldMaid::Card::NumberCard.new(4))
            .accept(OldMaid::Card::NumberCard.new(5))
            .get_ready.as_drawn,
          2,
        ],
        [
          'count of cards in hand is 4(even)',
          OldMaid::Player
            .prepare(name: 'test')
            .accept(OldMaid::Card::NumberCard.new(1))
            .accept(OldMaid::Card::NumberCard.new(2))
            .accept(OldMaid::Card::NumberCard.new(3))
            .accept(OldMaid::Card::NumberCard.new(4))
            .get_ready.as_drawn,
          1,
        ],
        [
          'count of cards in hand is 2',
          OldMaid::Player
            .prepare(name: 'test')
            .accept(OldMaid::Card::NumberCard.new(1))
            .accept(OldMaid::Card::NumberCard.new(2))
            .get_ready.as_drawn,
          0,
        ],
        [
          'count of cards in hand is 1',
          OldMaid::Player
            .prepare(name: 'test')
            .accept(OldMaid::Card::NumberCard.new(1))
            .get_ready.as_drawn,
          0,
        ],
      ]
    end

    with_them do
      it "will pick a card from #{params[:expected]}" do
        sut = OldMaid::Player::Strategy::AlwaysCenter.instance

        expect(sut.pick_from(drawn)).to eq expected
      end
    end
  end
end