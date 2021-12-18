# frozen_string_literal: true

require 'singleton'

module Utils
  module Maybe
    class << self
      def some(value)
        Some.new(value)
      end

      def none
        None.instance
      end
    end

    class Some
      def initialize(value)
        @value = value
      end

      def map
        Some.new(yield @value)
      end
    end

    class None
      include Singleton

      def map
        self
      end
    end
  end
end
