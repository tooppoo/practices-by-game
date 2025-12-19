
require 'rspec'

require_relative './hand'
require_relative './player'

describe 'Player' do
  before do
    class DummyStrategy
      def next_hand
        JankenMulti::Hand::PAPER
      end
    end

    @player = JankenMulti::Player.new(name: 'Alice', strategy: DummyStrategy.new)
  end

  it 'should have a name' do
    expect(@player.name).to eq 'Alice'
  end

  it 'should show hand following strategy' do
    expect(@player.show_hand).to eq JankenMulti::Hand::PAPER
  end
end
