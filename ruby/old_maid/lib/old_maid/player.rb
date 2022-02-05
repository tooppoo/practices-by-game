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

    def finished?
      self.class == State::Finished
    end

    private def transit_to(next_state, name: self.name, cards: self.cards)
      next_state.new(name: name, cards: cards)
    end

    module State
      module Acceptable
        def accept(card)
          next_cards = if cards.include?(card.to_sym)
                         cards.reject { |_, c| c == card }
                       else
                         cards.merge({ card.to_sym => card })
                       end

          transit_to state_after_accept(next_cards: next_cards), cards: next_cards
        end

        private def state_after_accept(next_cards:)
          raise NotImplementedError.new
        end
      end

      class Preparing < Player
        def get_ready
          if cards.empty?
            transit_to Finished
          else
            transit_to GetReady
          end
        end

        include Acceptable
        private def state_after_accept(next_cards:)
          Preparing
        end
      end

      class GetReady < Player
        protected def initialize(name:, cards:)
          raise ArgumentError.new("when a player be get-ready, the player must have at least one card") if cards.empty?

          super
        end

        def as_drawing
          transit_to Drawing
        end

        def as_drawn
          transit_to Drawn
        end
      end

      class Drawing < Player
        include Acceptable

        private def state_after_accept(next_cards:)
          if next_cards.empty?
            Finished
          else
            Drawn
          end
        end
      end

      class Drawn < Player
        TupleProvide = Struct.new(:card, :player)

        def provide(randomizer = Random.new)
          drawn = cards.values.sample(random: randomizer)
          cards_after_drawn = cards.reject { |_, card| card == drawn }

          next_state = if cards_after_drawn.empty?
                         Finished
                       else
                         Drawing
                       end
          player = transit_to next_state, cards: cards_after_drawn

          TupleProvide.new(drawn, player)
        end
      end
      
      class Finished < Player
        protected def initialize(name:, cards:)
          raise ArgumentError.new("when a player finished, the player can not any cards") unless cards.empty?

          super
        end

        private def transit_to(_)
          self
        end
      end
    end
  end
end
