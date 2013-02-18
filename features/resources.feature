Feature: Resources
  In order to control outbound calls
  As a developer
  I want to throttle resources

  Scenario: Get resources
    When I try to invoke /resources
    Then I should receive an HTTP Status of 200
    And I should receive valid JSON

  Scenario: Checkout a resource
    Given I put {"segment":"Service"} to /resources/checkout
    Then I should receive an HTTP Status of 200
    Then I should receive valid JSON
    And I should receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":14}

  Scenario: Checkin a resource
    Given I put {"segment":"Service"} to /resources/checkout
    When I put {"segment":"Service"} to /resources/checkin
    Then I should receive an HTTP Status of 200
    Then I should receive valid JSON
    And I should receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":15}
