# frozen_string_literal: true

module OldMaid
  class Player
    module State
      module Acceptable
        def accept(card)
          event_emitter.emit(Event::ACCEPT, self, card)

          next_cards = if cards_in_hand.include?(card)
                         event_emitter.emit(Event::DUMP, self, card)
                         cards_in_hand.dump card
                       else
                         cards_in_hand.add card
                       end

          transit_to(state_after_accept(next_cards: next_cards), cards_in_hand: next_cards)
        end

        private def state_after_accept(next_cards:)
          raise NotImplementedError.new
        end
      end

      class Preparing < Player
        def get_ready
          if cards_in_hand.empty?
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
        protected def initialize(name:, cards_in_hand:, event_emitter:)
          raise ArgumentError.new("when a player be get-ready, the player must have at least one card") if cards_in_hand.empty?

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
        TupleProvide = Struct.new(:card, :player) do
          def to_a
            [card, player]
          end
        end

        def skip_drawn
          transit_to Drawing
        end

        def provide(randomizer = Random.new)
          drawn = cards_in_hand.sample(random: randomizer)

          event_emitter.emit(Event::DRAWN, self, drawn)

          cards_after_drawn = cards_in_hand.dump drawn

          next_state = if cards_after_drawn.empty?
                         Finished
                       else
                         Drawing
                       end
          player = transit_to next_state, cards_in_hand: cards_after_drawn

          TupleProvide.new(drawn, player)
        end
      end

      class Finished < Player
        protected def initialize(name:, cards_in_hand:, event_emitter:)
          raise ArgumentError.new("when a player finished, the player can not any cards") unless cards_in_hand.empty?

          event_emitter.emit(Event::FINISH, self)

          super
        end

        private def transit_to(_)
          raise InvalidTransitionError.new("Finished is end of state. player can not transit to any state from Finished")
        end
      end
      class InvalidTransitionError < RuntimeError; end
    end
  end
end
