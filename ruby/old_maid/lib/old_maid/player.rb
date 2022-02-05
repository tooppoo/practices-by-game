# frozen_string_literal: true

module OldMaid
  class Player
    def self.prepare(name:)
      new(name: name, cards: {}).tap do |player|
        OldMaid::Player::State::Preparing.apply player
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

    def clone
      Player.new(name: name, cards: cards.dup)
    end

    module State
      module BaseState
        private def transit_to(next_state, &decorate)
          clone.tap do |c|
            c.extend next_state

            c.instance_eval &decorate if block_given?
          end
        end
      end

      module Preparing
        include BaseState

        def self.apply(player)
          player.extend self
        end

        def accept(card)
          transit_to Preparing do
            cards[card.to_sym] = card
          end
        end

        def get_ready
          transit_to(GetReady)
        end
      end
      module GetReady
        include BaseState

        def as_receiver
          transit_to Drawing
        end

        def as_provider
          transit_to Drawn
        end
      end
      module Drawing
        include BaseState

        def accept(card)
          transit_to Drawn do
            if cards.include?(card.to_sym)
              cards.delete card.to_sym
            else
              cards[card.to_sym] = card
            end
          end
        end
      end
      module Drawn
        include BaseState

        TupleProvide = Struct.new(:card, :player)

        def provide(randomizer = Random.new)
          drawn = cards.values.sample(random: randomizer)
          player = transit_to(Drawing) do
            cards.reject! { |_, card| card == drawn }
          end

          TupleProvide.new(drawn, player)
        end
      end
    end
  end
end
