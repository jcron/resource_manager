$LOAD_PATH.unshift(File.expand_path("features/support"))
require "conversation_tracker"

Before do
  @conversation_tracker = ConversationTracker.new
end

After do
  @conversation_tracker.CleanupConversation
end

