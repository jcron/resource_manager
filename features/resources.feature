Feature: Resources
  In order to control outbound calls
  As a developer
  I want to throttle resources

  Scenario: Get resources
    When I try to invoke /resources
    Then I should receive an HTTP Status of 200
    And I should receive valid JSON

  @cleanup
  Scenario Outline: Checkout a resource with json data
    Given I put <content_type> data <data> to /resources/checkout
    Then I should receive an HTTP Status of 200
    Then I should receive valid JSON
    And I should receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":14}
  Examples:
    | content_type                      | data                                       |
    | application/json                  | {"segment":"Service", "id":"conversation"} |
    | application/x-www-form-urlencoded | segment=Service&id=conversation            |

  Scenario Outline: Checkin a resource
    Given I put <content_type> data <data> to /resources/checkout
    When I put <content_type> data <data> to /resources/checkin
    Then I should receive an HTTP Status of 200
    Then I should receive valid JSON
    And I should receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":15}
  Examples:
    | content_type                      | data                                       |
    | application/json                  | {"segment":"Service", "id":"conversation"} |
    | application/x-www-form-urlencoded | segment=Service&id=conversation            |
