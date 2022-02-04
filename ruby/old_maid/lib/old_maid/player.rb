# frozen_string_literal: true

module OldMaid
  class Player
    def self.prepare(name:)
      new(name: name, cards: []).tap do |player|
        player.extend OldMaid::Player::State::Preparing
      end
    end

    attr_reader :name
    private attr_reader :cards

    protected def initialize(name:, cards:)
      invalid_chars = %W[< > # $ % \\ \t \n \u0000]

      raise ArgumentError.new("invalid characters contained") if invalid_chars.any? { |c| name.include? c }
      raise ArgumentError.new("not allowed empty player name") if name.empty?

      @name = name
      @cards = cards
    end

    def rest_cards
      cards.length
    end

    def copy
      Player.new(name: name, cards: cards.dup)
    end

    module State
      module Preparing
        def accept(card)
          copy.tap do |c|
            c.instance_eval { cards << card }
            c.extend Preparing
          end
        end

        def get_ready
          copy.tap do |c|
            c.extend GetReady
          end
        end
      end
      module GetReady
        def as_receiver
          copy.tap do |c|
            c.extend Drawing
          end
        end

        def as_provider
          copy.tap do |c|
            c.extend Drawn
          end
        end
      end
      module Drawing
        def accept(card)
          copy.tap do |c|
            c.instance_eval { cards << card }

            c.extend Drawn
          end
        end
      end
      module Drawn
        TupleProvide = Struct.new(:card, :player)

        def drawn(randomizer = Random.new)
          copy.instance_eval do
            index = randomizer.rand(0...rest_cards)

            drawn = cards[index]
            cards.reject! { |card| card == drawn }
            extend Drawing

            TupleProvide.new(drawn, self)
          end
        end
      end
    end
  end
end
