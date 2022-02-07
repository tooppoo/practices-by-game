# frozen_string_literal: true

require_relative './lib/old_maid'

log = []

begin
  players = [
    OldMaid::Player.prepare(name: 'player-1'),
    OldMaid::Player.prepare(name: 'player-2', draw_strategy: OldMaid::Player::Strategy::AlwaysHead.instance),
    OldMaid::Player.prepare(name: 'player-3', draw_strategy: OldMaid::Player::Strategy::AlwaysLast.instance),
    OldMaid::Player.prepare(name: 'player-4', draw_strategy: OldMaid::Player::Strategy::AlwaysCenter.instance),
    OldMaid::Player.prepare(name: 'player-5'),
  ].map do |player|
    player.on_dump_card { |p, card| log << "#{p.name} dumped #{card} from #{p.cards_in_hand.to_a}" }
          .on_drawn { |p, card| log << "#{p.name} drawn #{card} from #{p.cards_in_hand.to_a}" }
          .on_accept { |p, card| log << "#{p.name} accept #{card} in #{p.cards_in_hand.to_a}" }
          .on_finish { log << "#{player.name} finished" }
          .on_transit { |before, after| log << "#{player.name} transit from #{before.current.to_s} to #{after.current.to_s}" }
  end

  sut = OldMaid::Game.new(players: players)
                     .on_deal_start { log << "start dealing card to each player" }
                     .on_deal_finish { log << "finish dealing card to each player" }
                     .on_play_start { |players_in_game| log << "game start" << players_in_game.map(&:to_h) }
                     .on_play_finish { log << "game finish" }

  deck = OldMaid::Card::Deck.new_pack

  loser = sut.play(deck: deck)

  puts log, "#{loser.name} is loser"
rescue => ex
  puts ex, ex.backtrace, log
end
