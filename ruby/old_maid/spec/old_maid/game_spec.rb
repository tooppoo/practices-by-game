# frozen_string_literal: true

require 'rspec'
require 'rspec-parameterized'

RSpec.describe 'Game' do
  describe 'Turn' do
    describe 'proceed' do
      where(:case_name, :expected, :players_before, :players_after) do
        [
          [
            'both of drawer and drawn not finished',
            'both of them still stay as player',
            [
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawer,
              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::NumberCard.new(3))
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(4))
                             .get_ready.as_drawn,
            ],
            [

              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::NumberCard.new(3))
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(4))
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawer,
            ],
          ]
        ]
      end
    end
  end
end