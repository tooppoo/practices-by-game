# frozen_string_literal: true

module OldMaid
  class Player
    def self.prepare(name:)
      new(name: name, cards: {}).instance_eval do
        transit_to State::Preparing
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

    private def transit_to(next_state, name: self.name, cards: self.cards)
      next_state.new(name: name, cards: cards)
    end

    module State
      module Acceptable
        def accept(card)
          next_cards = if cards.include?(card.to_sym)
                         cards.reject { |k| k == card.to_sym }
                       else
                         cards.merge({ card.to_sym => card })
                       end

          transit_to state_after_accept, cards: next_cards
        end

        private def state_after_accept
          raise NotImplementedError.new
        end
      end

      class Preparing < Player
        def get_ready
          transit_to(GetReady)
        end

        include Acceptable
        private def state_after_accept
          Preparing
        end
      end

      class GetReady < Player
        def as_receiver
          transit_to Drawing
        end

        def as_provider
          transit_to Drawn
        end
      end

      class Drawing < Player
        include Acceptable

        private def state_after_accept
          Drawn
        end
      end

      class Drawn < Player
        TupleProvide = Struct.new(:card, :player)

        def provide(randomizer = Random.new)
          drawn = cards.values.sample(random: randomizer)
          player = transit_to Drawing, cards: cards.reject { |_, card| card == drawn }

          TupleProvide.new(drawn, player)
        end
      end
    end
  end
end
