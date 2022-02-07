# frozen_string_literal: true

require 'rspec'
require 'rspec-parameterized'
require_relative '../../lib/old_maid'

MockStrategy = Struct.new(:pick_at, keyword_init: true) do
  def pick_from(_)
    pick_at
  end
end

RSpec.describe 'Game' do
  describe 'Turn' do
    describe 'proceed' do
      where(:case_name, :expected, :players_before, :players_after) do
        [
          [
            'both of drawer and drawn not finished',
            'keep both of them in players list',
            [
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawer.tap {|a|
                               def a.draw_strategy
                                 MockStrategy.new(pick_at: 0)
                               end
                             },
              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn,
            ],
            [
              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawer,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .get_ready.as_drawn,
            ],
          ],
          [
            'both of drawer and drawn finished',
            'remove both of them from players list',
            [
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawer,
              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn,
            ],
            [
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn,
            ],
          ],
          [
            'drawer finished but drawn not',
            'remove only drawer from players list',
            [
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawer.tap { |a|
                               def a.draw_strategy
                                 MockStrategy.new(pick_at: 0)
                               end
                             },
              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn,
            ],
            [
              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .get_ready.as_drawer,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn,
            ],
          ],
          [
            'drawn finished but drawer not',
            'remove only drawn from players list and skip drawn of next player',
            [
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .get_ready.as_drawer,
              OldMaid::Player.prepare(name: 'b')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawn,
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn,
            ],
            [
              OldMaid::Player.prepare(name: 'c')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .accept(OldMaid::Card::Joker.instance)
                             .get_ready.as_drawn.skip_drawn,
              OldMaid::Player.prepare(name: 'a')
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .get_ready.as_drawn,
            ],
          ],
        ]
      end

      with_them do
        it "should #{params[:expected]}" do
          expect(OldMaid::Game::Turn.proceed(players_before)).to eq players_after
        end
      end
    end
  end
end