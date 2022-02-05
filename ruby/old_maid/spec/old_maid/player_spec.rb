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
      preparing = OldMaid::Player.prepare(name: 'test')

      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
      preparing = preparing.accept OldMaid::Card::NumberCard.new(2)
      sut = preparing.accept OldMaid::Card::NumberCard.new(3)

      expect(sut.rest_cards).to eq 3
    end

    it 'should dump same cards within accepting' do
      preparing = OldMaid::Player.prepare(name: 'test')

      preparing = preparing.accept OldMaid::Card::NumberCard.new(2)
      preparing = preparing.accept OldMaid::Card::NumberCard.new(2)
      preparing = preparing.accept OldMaid::Card::NumberCard.new(2)

      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
      sut = preparing.accept OldMaid::Card::NumberCard.new(1)

      expect(sut.rest_cards).to eq 1
    end

    it 'not accept cards after get-ready' do
      preparing = OldMaid::Player.prepare(name: 'test')

      sut = preparing.get_ready

      expect(sut).not_to respond_to(:accept)
    end
  end

  context 'behave as drawer' do
    describe 'draw a card' do
      context 'the player not has yet' do
        it 'can accept a card and keep all cards in hand' do
          preparing = OldMaid::Player.prepare(name: 'test')
          preparing = preparing.accept OldMaid::Card::NumberCard.new(1)

          player = preparing.get_ready
          drawer = player.as_drawing

          sut = drawer.accept OldMaid::Card::NumberCard.new(3)

          expect(sut.rest_cards).to eq 2
        end
      end
      context 'the player has already' do
        it 'can accept a card and dump same cards' do
          preparing = OldMaid::Player.prepare(name: 'test')
          preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
          preparing = preparing.accept OldMaid::Card::NumberCard.new(2)
          preparing = preparing.accept OldMaid::Card::NumberCard.new(3)

          player = preparing.get_ready
          drawer = player.as_drawing

          sut = drawer.accept OldMaid::Card::NumberCard.new(3)

          expect(sut.rest_cards).to eq 2
        end
      end
    end

    it 'can not accept a card twice' do
      preparing = OldMaid::Player.prepare(name: 'test')
      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)

      player = preparing.get_ready
      drawer = player.as_drawing

      sut = drawer.accept OldMaid::Card::NumberCard.new(3)

      expect(sut).not_to respond_to(:accept)
    end
  end

  context 'behave as drawn' do
    it 'provide a card' do
      preparing = OldMaid::Player.prepare(name: 'test')

      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
      sut = preparing.get_ready.as_drawn

      expect(sut.provide.card).to eq OldMaid::Card::NumberCard.new(1)
    end
    it 'should decrease cards in hand after provide' do
      preparing = OldMaid::Player.prepare(name: 'test')

      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
      drawn = preparing.get_ready.as_drawn
      sut = drawn.provide.player

      expect(sut.rest_cards).to eq 0
    end
    it 'can not provide a card twice' do
      preparing = OldMaid::Player.prepare(name: 'test')

      preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
      drawn = preparing.get_ready.as_drawn
      sut = drawn.provide.player

      expect(sut).not_to respond_to(:provide)
    end
  end

  it 'can provide, draw and provide a card' do
    preparing = OldMaid::Player.prepare(name: 'test')

    preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
    preparing = preparing.accept OldMaid::Card::NumberCard.new(2)

    sut = preparing.get_ready.as_drawn
    sut = sut.provide.player
    sut = sut.accept OldMaid::Card::NumberCard.new(3)
    sut = sut.provide.player

    expect(sut.rest_cards).to eq 1
  end

  it 'can draw, provide and draw a card' do
    preparing = OldMaid::Player.prepare(name: 'test')

    preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
    preparing = preparing.accept OldMaid::Card::NumberCard.new(2)

    sut = preparing.get_ready.as_drawing
    sut = sut.accept OldMaid::Card::NumberCard.new(3)
    sut = sut.provide.player
    sut = sut.accept OldMaid::Card::NumberCard.new(4)

    expect(sut.rest_cards).to eq 3
  end

  context 'player\'s hand become empty' do
    context 'on drawn' do
      it 'player become finishing state' do
        preparing = OldMaid::Player.prepare(name: 'test')
        preparing = preparing.accept OldMaid::Card::NumberCard.new(1)

        drawn = preparing.get_ready.as_drawn
        sut = drawn.provide.player

        expect(sut.finished?).to be true
      end
    end

    context 'on draw' do
      it 'player become finishing state' do
        preparing = OldMaid::Player.prepare(name: 'test')
        preparing = preparing.accept OldMaid::Card::NumberCard.new(1)

        drawer = preparing.get_ready.as_drawing
        sut = drawer.accept OldMaid::Card::NumberCard.new(1)

        expect(sut.finished?).to be true
      end
    end

    context 'on get-ready, a player already has no cards' do
      it 'player become finishing state' do
        preparing = OldMaid::Player.prepare(name: 'test')
        preparing = preparing.accept OldMaid::Card::NumberCard.new(1)
        preparing = preparing.accept OldMaid::Card::NumberCard.new(1)

        sut = preparing.get_ready

        expect(sut.finished?).to be true
      end
    end
  end
end