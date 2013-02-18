require 'rest_client'
require 'json'

When /^I try to invoke (.*?)$/ do |url|
  begin
    @response = RestClient.get(TARGET + url,{:accept => :json})
    @code = @response.code
  rescue RestClient::Exception => e
    @code = e.http_code
  end
end

When /^I put (.*) data (.*) to (.*?)$/ do |content_type, body, url|
  begin
    @conversation_tracker.AddConversation(body, content_type) if @conversation_tracker
    @response = RestClient.put(TARGET + url, body, {:accept => :json, :content_type => content_type})
    @code = @response.code
  rescue RestClient::Exception => e
    @code = e.http_code
  end
end

Then /^I should receive an HTTP Status of (.*)$/ do |status|
  @code.to_s.should == status
end

Then /^I should receive valid JSON$/ do
  @parsed = JSON.parse(@response.body)
end

When /^I should receive a body with a segment of ({.*})$/ do |segment|
  @parsed["segments"].should == JSON.parse(segment)
end
