require "rest_client"

class ConversationTracker
  attr_reader :conversation_list
  def initialize
    @conversation_list = []
  end

  def AddConversation(body, content_type)
    @conversation_list << [body, content_type]
  end

  def CleanupConversation
    while conversation = @conversation_list.pop do
      RestClient.put(TARGET + "/resources/checkin", conversation[0], {:accept => :json, :content_type => conversation[1]})
    end
  end
end