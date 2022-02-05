# frozen_string_literal: true

require_relative './dealer'
require_relative './card'
require_relative '../util/event_emitter'

module OldMaid
  class Game
    private attr_reader :players, :event_emitter

    def initialize(players:)
      raise ArgumentError.new("player must exist at least 2") if players.length < 2

      @players = players
      @event_emitter = OldMaid::Util::EventEmitter.new
    end

    def on_deal_start(&handler)
      tap do
        event_emitter.on(Event::Deal::START, &handler)
      end
    end
    def on_deal_finish(&handler)
      tap do
        event_emitter.on(Event::Deal::FINISH, &handler)
      end
    end
    def on_play_start(&handler)
      tap do
        event_emitter.on(Event::Play::START, &handler)
      end
    end
    def on_play_finish(&handler)
      tap do
        event_emitter.on(Event::Play::FINISH, &handler)
      end
    end

    def play(deck:)
      dealer = OldMaid::Dealer.new(deck)

      event_emitter.emit(Event::Deal::START)
      first, *rest = dealer.deal_to(players.shuffle).reject { |p| p.finished? }
      event_emitter.emit(Event::Deal::FINISH)

      players = [first.as_drawing, *rest.map(&:as_drawn)]

      event_emitter.emit(Event::Play::START, players)
      last, = __play(players: players)
      event_emitter.emit(Event::Play::FINISH)

      last
    end

    private def __play(players:)
      players_mut = players

      while players_mut.length > 1 do
        drawer, drawn, *rest = players_mut

        card, drawn_after = drawn.provide.to_a
        drawer_after = drawer.accept card

        players_mut = [drawn_after, *rest, drawer_after].reject { |p| p.finished? }
      end

      players_mut
    end
  end

  module Event
    module Deal
      START = 'on_deal_start'
      FINISH = 'on_deal_finish'
    end
    module Play
      START = 'on_game_start'
      FINISH = 'on_game_finish'
    end
  end
end
