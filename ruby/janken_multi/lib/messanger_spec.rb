
require 'rspec'
require_relative './messanger'

describe 'Messanger' do
  before do
    class Sender
      include JankenMulti::Messanger::MessageSender
    end

    class Receiver1
      include JankenMulti::Messanger::MessageReceiver
    end
    class Receiver2
      include JankenMulti::Messanger::MessageReceiver
    end

    @sender = Sender.new
    @receiver1_1 = Receiver1.new
    @receiver1_2 = Receiver1.new
    @receiver2 = Receiver2.new
  end

  it 'should send and receive message' do
    received_message1 = nil
    received_message2 = nil
    @receiver1_1.listen_message do |message|
      received_message1 = message
    end
    @receiver1_2.listen_message do |message|
      received_message1 += 'No2'
    end

    @receiver2.listen_message do
      received_message2 = 'received'
    end

    @sender.send_message(Receiver1, 'Hello, World!')

    expect(received_message1).to eq 'Hello, World!No2'
    expect(received_message2).to eq nil
  end
end