
require 'singleton'

module JankenMulti
  module Messanger
    module MessageSender
      def send_message(target, message)
        MailBox.instance.send_message(target, message)
      end
    end
    module MessageReceiver
      def listen_message(&block)
        MailBox.instance.listen_message(self.class, &block)
      end
    end

    class MailBox
      include Singleton

      def initialize
        @listeners = {}
      end

      def send_message(target, message)
        if @listeners.key?(target)
          @listeners[target].each do |listener|
            listener.call(message)
          end
        end
      end

      def listen_message(target, &block)
        @listeners[target] ||= []
        @listeners[target] << block
      end
    end
  end
end
