# frozen_string_literal: true

module OldMaid
  module Util
    class EventEmitter
      private attr_reader :handlers

      def initialize
        @handlers = {}
      end

      def on(event, &handler)
        tap do
          handlers[event] = handlers.fetch(event, []) .push handler
        end
      end
      def emit(event, *args)
        tap do
          handlers.fetch(event, []).each do |handler|
            handler.call(*args)
          end
        end
      end
    end
  end
end
