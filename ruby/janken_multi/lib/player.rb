
module JankenMulti
  class Player
    attr_reader :name, :win_count, :lose_count

    def initialize(name:, strategy:)
      @name = name
      @strategy = strategy

      @win_count = 0
      @lose_count = 0
    end

    def show_hand
      @strategy.next_hand
    end

    def notify_win!
      @win_count += 1
    end 
    def notify_lose!
      @lose_count += 1
    end
  end
end