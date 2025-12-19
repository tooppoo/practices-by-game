
require 'rspec'
require 'rspec-parameterized'
require_relative './judge'
require_relative './hand'

describe 'JankenMulti::Judge' do
  describe 'judge' do
    before do
      class Player; end

      @player1 = Player.new
      @player2 = Player.new
      @player3 = Player.new
    end

    where(:hands, :result) do
      [
        [
          [
            { hand: JankenMulti::Hand::STONE, player: @player1 },
            { hand: JankenMulti::Hand::STONE, player: @player2 },
          ],
          JankenMulti::Judge::Result.draw,
        ],
        [
          [
            { hand: JankenMulti::Hand::PAPER, player: @player1 },
            { hand: JankenMulti::Hand::PAPER, player: @player2 }, 
          ],
          JankenMulti::Judge::Result.draw,
        ],
        [
          [
            { hand: JankenMulti::Hand::SCISSORS, player: @player1 },
            { hand: JankenMulti::Hand::SCISSORS, player: @player2 },
          ],
          JankenMulti::Judge::Result.draw,
        ],
        [
          [
            { hand: JankenMulti::Hand::STONE, player: @player1 },
            { hand: JankenMulti::Hand::PAPER, player: @player2 },
          ],
          JankenMulti::Judge::Result.new(winners: [@player1], losers: [@player2]),
        ],
        [
          [
            { hand: JankenMulti::Hand::PAPER, player: @player1 },
            { hand: JankenMulti::Hand::SCISSORS, player: @player2 },
          ],
          JankenMulti::Judge::Result.new(winners: [@player2], losers: [@player1] ),
        ],
        [
          [
            { hand: JankenMulti::Hand::SCISSORS, player: @player1 },
            { hand: JankenMulti::Hand::STONE, player: @player2 },
          ],
          JankenMulti::Judge::Result.new(winners: [@player2], losers: [@player1] ),
        ],
        [
          [
            { hand: JankenMulti::Hand::STONE, player: @player1 },
            { hand: JankenMulti::Hand::PAPER, player: @player2 },
            { hand: JankenMulti::Hand::SCISSORS, player: @player3 },
          ],
          JankenMulti::Judge::Result.draw,
        ],
        [
          [
            { hand: JankenMulti::Hand::STONE, player: @player1 },
            { hand: JankenMulti::Hand::STONE, player: @player2 },
            { hand: JankenMulti::Hand::PAPER, player: @player3 },
          ],
          JankenMulti::Judge::Result.new(winners: [@player3], losers: [@player1, @player2]),
        ],
        [
          [
            { hand: JankenMulti::Hand::PAPER, player: @player1 },
            { hand: JankenMulti::Hand::PAPER, player: @player2 },
            { hand: JankenMulti::Hand::SCISSORS, player: @player3 },
          ],
          JankenMulti::Judge::Result.new(winners: [@player3], losers: [@player1, @player2]),
        ],
        [
          [
            { hand: JankenMulti::Hand::SCISSORS, player: @player1 },
            { hand: JankenMulti::Hand::STONE, player: @player2 },
            { hand: JankenMulti::Hand::STONE, player: @player3 },
          ],
          JankenMulti::Judge::Result.new(winners: [@player2, @player3], losers: [@player1]),
        ],
      ]
    end
    with_them do
      it 'should judge correctly' do
        input = JankenMulti::Judge::Input.new
        hands.each do |hand_player|
          input.add(player: hand_player[:player], hand: hand_player[:hand])
        end

        judge = JankenMulti::Judge.new
        expect(judge.results(input)).to eq result
      end
    end
  end
end
