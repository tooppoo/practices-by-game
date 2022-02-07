# frozen_string_literal: true

module OldMaid
  module Util
    class Tuple
      attr_reader :_a, :_b

      def initialize(a, b)
        @_a = a
        @_b = b
      end

      def to_a
        [_a, _b]
      end
    end
  end
end
