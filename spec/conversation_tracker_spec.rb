require "rspec"
require "spec_helper"
require "rest_client"
require "conversation_tracker"

describe ConversationTracker do

  TARGET = ENV["TARGET"] || "localhost:8000"

  it "should instantiate" do
    ConversationTracker.new
  end

  it "should add a conversation location and content_type" do
    it = ConversationTracker.new
    location = "Some Location"
    content_type = "application/json"
    it.AddConversation(location, content_type)
    it.conversation_list[0].should == [location, content_type]
  end

  it "should delete all conversations currently tracked" do
    it = ConversationTracker.new
    location1 = "location one"
    content_type1 = "application/json"
    location2 = "location two"
    content_type2 = "application/x-www-form-urlencoded"
    headers1 = {:accept=>:json, :content_type=>content_type1}
    headers2 = {:accept=>:json, :content_type=>content_type2}
    path = TARGET + "/resources/checkin"

    RestClient.should_receive(:put).with(path, location1, headers1)
    RestClient.should_receive(:put).with(path, location2, headers2)

    it.AddConversation(location1, content_type1)
    it.AddConversation(location2, content_type2)
    it.CleanupConversation

    it.conversation_list.should == []
  end
end