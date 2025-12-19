
module JankenMulti
  module Hand
    class Stone
      def to_s
        "stone"
      end
      def ==(other)
        other.is_a?(Stone)
      end
    end
    class Paper
      def to_s
        "paper" 
      end
      def ==(other)
        other.is_a?(Paper)
      end
    end
    class Scissors
      def to_s
        "scissors"
      end
      def ==(other)
        other.is_a?(Scissors)
      end
    end

    STONE = Stone.new
    PAPER = Paper.new
    SCISSORS = Scissors.new
  end
end
