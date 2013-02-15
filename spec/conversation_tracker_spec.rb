require "rspec"
require "spec_helper"
require "rest_client"
require "conversation_tracker"

describe ConversationTracker do

  it "should instantiate" do
    ConversationTracker.new
  end

  it "should add an conversation location" do
    it = ConversationTracker.new
    location = "Some Location"
    it.AddConversation(location)
    it.conversation_list[0].should == location
  end

  it "should delete all conversations currently tracked" do
    it = ConversationTracker.new
    location1 = "location one"
    location2 = "location two"
    headers = {:accept=>:json, :content_type=>:json}
    path = TARGET + "/resources/checkin"
    
    RestClient.should_receive(:put).with(path, location1, headers)
    RestClient.should_receive(:put).with(path, location2, headers)

    it.AddConversation(location1)
    it.AddConversation(location2)
    it.CleanupConversation

    it.conversation_list.should == []
  end
end