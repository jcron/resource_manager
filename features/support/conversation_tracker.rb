require "rest_client"

class ConversationTracker
  attr_reader :conversation_list
  def initialize
    @conversation_list = []
  end

  def AddConversation(location)
    @conversation_list << location
  end

  def CleanupConversation
    while conversation = @conversation_list.pop do
      RestClient.put(TARGET + "/resources/checkin", conversation, {:accept => :json, :content_type => :json})
    end
  end
end