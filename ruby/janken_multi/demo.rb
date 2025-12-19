
require_relative './lib/game'
require_relative './lib/player'
require_relative './lib/strategy'

players = []
players << JankenMulti::Player.new(name: 'Madoka', strategy: JankenMulti::Strategy::RandomStrategy.new)
players << JankenMulti::Player.new(name: 'Homura', strategy: JankenMulti::Strategy::RandomStrategy.new)
players << JankenMulti::Player.new(name: 'Sayaka', strategy: JankenMulti::Strategy::RandomStrategy.new)
players << JankenMulti::Player.new(name: 'Mami', strategy: JankenMulti::Strategy::RandomStrategy.new)
players << JankenMulti::Player.new(name: 'Kyoko', strategy: JankenMulti::Strategy::RandomStrategy.new)
players << JankenMulti::Player.new(name: 'QB', strategy: JankenMulti::Strategy::OrderStrategy.new([
  JankenMulti::Hand::STONE,
  JankenMulti::Hand::PAPER,
  JankenMulti::Hand::SCISSORS,
]))
game = JankenMulti::Game.new(players: players)

game.play_round!(times: 5) do |result|
  puts "Round Result:"
  result.when_draw do
    puts 'Draw!'
    puts '---'
  end
  result.when_not_draw do |res|
    puts "Winners: #{res.winners.map(&:name).join(', ')}"
    puts "Losers: #{res.losers.map(&:name).join(', ')}"
    puts '---'
  end
end

puts 'Final Scores:'
players.each do |player|
  puts "#{player.name} - Wins: #{player.win_count}, Losses: #{player.lose_count}"
end
