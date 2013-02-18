$LOAD_PATH.unshift(File.expand_path("features/support"))
require "conversation_tracker"

Before('@cleanup') do
  @conversation_tracker = ConversationTracker.new
end

After('@cleanup') do
  @conversation_tracker.CleanupConversation
end

