# frozen_string_literal: true

require 'rspec'
require 'rspec-parameterized'
require_relative '../../lib/old_maid'

RSpec.describe 'Player' do
  context 'name' do
    context 'invalid name' do
      where(:case_name, :name) do
        [
          ['contain <', "Ulike<Owen"],
          ['contain >', "Ulike>Owen"],
          ['contain \\', "Ulike\\Owen"],
          ['contain \n', "Ulike\\Owen"],
          ['contain \t', "Ulike\tOwen"],
          ['contain #', "Ulike#Owen"],
          ['contain $', "Ulike$Owen"],
          ['contain %', "Ulike%Owen"],
          ['contain null byte', "Ulike\u0000Owen"],
          ['empty name', ""],
          ['only blank char', " 　\t"],
        ]
      end

      with_them do
        it 'should not be allowed' do
          expect { OldMaid::Player.prepare(name: name) }.to raise_error ArgumentError
        end
      end
    end
  end

  context 'preparing' do
    it 'can accept cards' do
      sut = OldMaid::Player.prepare(name: 'test')
                           .accept(OldMaid::Card::NumberCard.new(1))
                           .accept(OldMaid::Card::NumberCard.new(2))
                           .accept(OldMaid::Card::NumberCard.new(3))

      expect(sut.rest_cards).to eq 3
    end

    it 'should dump same cards within accepting' do
      sut = OldMaid::Player.prepare(name: 'test')
                           .accept(OldMaid::Card::NumberCard.new(2))
                           .accept(OldMaid::Card::NumberCard.new(2))
                           .accept(OldMaid::Card::NumberCard.new(2))
                           .accept(OldMaid::Card::NumberCard.new(1))
                           .accept(OldMaid::Card::NumberCard.new(1))
                           .accept(OldMaid::Card::NumberCard.new(1))
                           .accept(OldMaid::Card::NumberCard.new(1))

      expect(sut.rest_cards).to eq 1
    end

    it 'not accept cards after get-ready' do
      preparing = OldMaid::Player.prepare(name: 'test')

      sut = preparing.get_ready

      expect(sut).not_to respond_to(:accept)
    end
  end

  context 'behave as drawer' do
    let(:drawer) {
      OldMaid::Player.prepare(name: 'drawer')
                     .accept(OldMaid::Card::NumberCard.new(1))
                     .accept(OldMaid::Card::NumberCard.new(2))
                     .get_ready.as_drawer
    }

    describe 'draw a card' do
      context 'the player not has yet' do
        let(:drawn) {
          OldMaid::Player.prepare(name: 'drawn')
                         .accept(OldMaid::Card::NumberCard.new(3))
                         .get_ready.as_drawn
        }

        it 'accept the card' do
          sut,_ = drawer.draw_from(drawn).to_a

          expect(sut.rest_cards).to eq drawer.rest_cards + 1
        end
      end
      context 'the player has already' do
        let(:drawn) {
          OldMaid::Player.prepare(name: 'drawn')
                         .accept(OldMaid::Card::NumberCard.new(1))
                         .get_ready.as_drawn
        }

        it 'dump the card with same' do
          sut,_ = drawer.draw_from(drawn).to_a

          expect(sut.rest_cards).to eq drawer.rest_cards - 1
        end
      end
    end

    it 'can not draw a card twice in a row' do
      drawer = OldMaid::Player.prepare(name: 'drawer')
                              .accept(OldMaid::Card::NumberCard.new(1))
                              .get_ready.as_drawer
      drawn = OldMaid::Player.prepare(name: 'drawn')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .accept(OldMaid::Card::NumberCard.new(2))
                             .get_ready.as_drawn

      sut,_ = drawer.draw_from(drawn).to_a

      expect(sut).not_to respond_to(:draw_from)
    end
  end

  context 'behave as drawn' do
    it 'be drawn a card' do
      drawn = OldMaid::Player.prepare(name: 'test')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawn

      card,_ = drawn.provide(at: 0).to_a

      expect(card).to eq OldMaid::Card::NumberCard.new(1)
    end
    it 'should decrease cards in hand after drawn' do
      drawn = OldMaid::Player.prepare(name: 'test')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawn

      _,sut = drawn.provide(at: 0).to_a

      expect(sut.rest_cards).to eq 0
    end
    it 'can not drawn a card twice in a row' do
      drawn = OldMaid::Player.prepare(name: 'test')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready.as_drawn

      _,sut = drawn.provide(at: 0).to_a

      expect(sut).not_to respond_to(:provide)
    end
  end

  it 'can swap cards with each other player' do
    player_a = OldMaid::Player.prepare(name: 'a')
                              .accept(OldMaid::Card::NumberCard.new(1))
                              .accept(OldMaid::Card::NumberCard.new(2))
                              .get_ready.as_drawer
    player_b = OldMaid::Player.prepare(name: 'b')
                              .accept(OldMaid::Card::NumberCard.new(3))
                              .accept(OldMaid::Card::NumberCard.new(4))
                              .get_ready.as_drawn

    player_a, player_b = player_a.draw_from(player_b).to_a
    player_b, player_a = player_b.draw_from(player_a).to_a

    expect([player_a.rest_cards, player_b.rest_cards]).to eq [2, 2]
  end

  context 'player\'s hand become empty' do
    context 'on drawn' do
      it 'player become finished state' do
        drawn = OldMaid::Player.prepare(name: 'test')
                               .accept(OldMaid::Card::NumberCard.new(1))
                               .get_ready.as_drawn

        _,sut = drawn.provide(at: 0).to_a

        expect(sut.finished?).to be true
      end
    end

    context 'on draw' do
      it 'player become finished state' do
        drawer = OldMaid::Player.prepare(name: 'drawer')
                                .accept(OldMaid::Card::NumberCard.new(1))
                                .get_ready.as_drawer
        drawn = OldMaid::Player.prepare(name: 'drawn')
                               .accept(OldMaid::Card::NumberCard.new(1))
                               .get_ready.as_drawn

        sut,_ = drawer.draw_from(drawn).to_a

        expect(sut.finished?).to be true
      end
    end

    context 'on get-ready, a player already has no cards' do
      it 'player become finished state' do
        sut = OldMaid::Player.prepare(name: 'test')
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .accept(OldMaid::Card::NumberCard.new(1))
                             .get_ready

        expect(sut.finished?).to be true
      end
    end
  end
end