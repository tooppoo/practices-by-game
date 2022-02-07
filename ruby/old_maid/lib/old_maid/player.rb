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

    def ==(other)
      name == other.name \
      && cards_in_hand == other.cards_in_hand \
      && current == other.current
    end

    def rest_cards
      cards_in_hand.length
    end

    def current
      self.class
    end

    def finished?
      current == State::Finished
    end

    def to_h
      {
        name: name,
        cards_in_hand: cards_in_hand.to_a,
        current: current.to_s,
      }
    end

    def on_transit(&handler)
      tap do
        event_emitter.on(Event::TRANSIT, &handler)
      end
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

    private def transit_to(next_state, name: self.name, cards_in_hand: self.cards_in_hand)
      next_state.new(
        name: name,
        cards_in_hand: cards_in_hand,
        event_emitter: event_emitter
      ).tap do |transited|
        event_emitter.emit(Event::TRANSIT, self, transited)
      end
    end

    module Event
      GET_READY = 'on_get_ready'
      DUMP = 'on_dump'
      ACCEPT = 'on_accept'
      DRAWN = 'on_drawn'
      FINISH = 'on_finish'
      TRANSIT = 'on_transit'
    end

    require_relative './player/cards_in_hand'
    require_relative './player/state'
  end
end
