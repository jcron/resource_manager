require "rspec"
require "spec_helper"
require "rest_client"
require "conversation_tracker"

describe ConversationTracker do

  TARGET = ENV["TARGET"] || "localhost:8000"

  it "should instantiate" do
    ConversationTracker.new
  end

  it "should add a conversation body and content_type" do
    it = ConversationTracker.new
    body = "Some data"
    content_type = "application/json"
    it.AddConversation(body, content_type)
    it.conversation_list[0].should == [body, content_type]
  end

  it "should delete all conversations currently tracked" do
    it = ConversationTracker.new
    body1 = "body one"
    content_type1 = "application/json"
    body2 = "body two"
    content_type2 = "application/x-www-form-urlencoded"
    headers1 = {:accept=>:json, :content_type=>content_type1}
    headers2 = {:accept=>:json, :content_type=>content_type2}
    path = TARGET + "/resources/checkin"

    RestClient.should_receive(:put).with(path, body1, headers1)
    RestClient.should_receive(:put).with(path, body2, headers2)

    it.AddConversation(body1, content_type1)
    it.AddConversation(body2, content_type2)
    it.CleanupConversation

    it.conversation_list.should == []
  end
end