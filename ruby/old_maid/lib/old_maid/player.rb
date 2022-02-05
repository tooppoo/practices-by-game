# frozen_string_literal: true

module OldMaid
  class Player
    def self.prepare(name:)
      new(
        name: name,
        cards_in_hand: CardsInHand.empty,
        event_emitter: OldMaid::Util::EventEmitter.new
      ).instance_eval do
        transit_to State::Preparing
      end
    end

    attr_reader :name, :cards_in_hand
    private attr_reader :event_emitter

    protected def initialize(name:, cards_in_hand:, event_emitter:)
      invalid_chars = %W[< > # $ % \\ \t \n \u0000]

      raise ArgumentError.new("invalid characters contained") if invalid_chars.any? { |c| name.include? c }
      raise ArgumentError.new("not allowed empty player name") if name.empty?

      @name = name
      @cards_in_hand = cards_in_hand
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

    def rest_cards
      cards_in_hand.length
    end

    def finished?
      self.class == State::Finished
    end

    def to_h
      {
        name: name,
        cards_in_hand: cards_in_hand.to_a,
      }
    end

    private def transit_to(next_state, name: self.name, cards_in_hand: self.cards_in_hand)
      next_state.new(
        name: name,
        cards_in_hand: cards_in_hand,
        event_emitter: event_emitter
      )
    end

    module Event
      GET_READY = 'on_get_ready'
      DUMP = 'on_dump'
      ACCEPT = 'on_accept'
      DRAWN = 'on_drawn'
      FINISH = 'on_finish'
    end

    class CardsInHand
      def self.empty
        new({})
      end

      private attr_reader :cards

      protected def initialize(cards)
        @cards = cards
      end

      def sample(random:)
        cards.values.sample(random: random)
      end
      def length
        cards.length
      end
      def empty?
        cards.empty?
      end

      def to_a
        cards.values.map(&:to_s)
      end

      def dump(card)
        with cards.reject { |_, c| c == card }
      end

      def include?(card)
        cards.include?(card.to_sym)
      end

      def add(card)
        with cards.merge({ card.to_sym => card })
      end

      private def with(cards)
        CardsInHand.new(cards)
      end
    end

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
