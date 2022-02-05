# frozen_string_literal: true

module OldMaid
  class Player
    def self.prepare(name:)
      new(name: name, cards: {}, event_emitter: OldMaid::Util::EventEmitter.new).instance_eval do
        transit_to State::Preparing
      end
    end

    attr_reader :name
    private attr_reader :cards, :event_emitter

    protected def initialize(name:, cards:, event_emitter:)
      invalid_chars = %W[< > # $ % \\ \t \n \u0000]

      raise ArgumentError.new("invalid characters contained") if invalid_chars.any? { |c| name.include? c }
      raise ArgumentError.new("not allowed empty player name") if name.empty?

      @name = name
      @cards = cards
      @event_emitter = event_emitter
    end

    def on_dump_card(&handler)
      tap do
        event_emitter.on(Event::DUMP, &handler)
      end
    end
    def on_accept(&handler)
      tap do
        event_emitter.on(Event::ACCEPT, &handler)
      end
    end
    def on_drawn(&handler)
      tap do
        event_emitter.on(Event::DRAWN, &handler)
      end
    end
    def on_finish(&handler)
      tap do
        event_emitter.on(Event::FINISH, &handler)
      end
    end

    def ==(other)
      name == other.name
    end

    def cards_in_hand
      cards.dup.freeze
    end

    def rest_cards
      cards.length
    end

    def finished?
      self.class == State::Finished
    end

    def to_h
      {
        name: name,
        cards: cards,
      }
    end

    private def cards_without(card)
      cards.reject { |_, c| c == card }
    end

    private def transit_to(next_state, name: self.name, cards: self.cards)
      next_state.new(name: name, cards: cards, event_emitter: event_emitter)
    end

    module Event
      GET_READY = 'on_get_ready'
      DUMP = 'on_dump'
      ACCEPT = 'on_accept'
      DRAWN = 'on_drawn'
      FINISH = 'on_finish'
    end

    module State
      module Acceptable
        def accept(card)
          event_emitter.emit(Event::ACCEPT, self, card)

          next_cards = if cards.include?(card.to_sym)
                         event_emitter.emit(Event::DUMP, self, card)
                         cards_without card
                       else
                         cards.merge({ card.to_sym => card })
                       end

          transit_to(state_after_accept(next_cards: next_cards), cards: next_cards)
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
        protected def initialize(name:, cards:, event_emitter:)
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
        TupleProvide = Struct.new(:card, :player) do
          def to_a
            [card, player]
          end
        end

        def provide(randomizer = Random.new)
          drawn = cards.values.sample(random: randomizer)

          event_emitter.emit(Event::DRAWN, self, drawn)

          cards_after_drawn = cards_without drawn

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
        protected def initialize(name:, cards:, event_emitter:)
          raise ArgumentError.new("when a player finished, the player can not any cards") unless cards.empty?

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
