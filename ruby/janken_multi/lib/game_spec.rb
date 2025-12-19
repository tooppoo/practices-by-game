
require 'rspec'

require_relative './hand'
require_relative './game'
require_relative './player'

describe 'Game' do
  before do
    class DummyStrategy1
      def next_hand
        JankenMulti::Hand::STONE
      end
    end

    class DummyStrategy2
      def next_hand
        JankenMulti::Hand::PAPER
      end
    end

    @player1 = JankenMulti::Player.new(name: 'Alice', strategy: DummyStrategy1.new)
    @player2 = JankenMulti::Player.new(name: 'Bob', strategy: DummyStrategy2.new)
    @player3 = JankenMulti::Player.new(name: 'Nancy', strategy: DummyStrategy2.new)

    @game = JankenMulti::Game.new(players: [@player1, @player2, @player3])
  end

  it 'should play a game and return results' do
    result = @game.play_round!(times: 3)

    expect(@player2.win_count).to eq 3
    expect(@player3.win_count).to eq 3
    expect(@player1.lose_count).to eq 3
  end
end